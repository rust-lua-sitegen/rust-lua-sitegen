use mlua::{AsChunk, FromLua, Lua, Table, UserData, Value};
use std::sync::Arc;

// Must be in sorted order, see https://html.spec.whatwg.org/multipage/syntax.html#elements-2
const VOID_ELEMS: &[&str] = &[
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

#[derive(Clone, Debug)]
enum HtmlKind {
    Elem {
        name: String,
        attrs: Vec<(String, String)>,
        content: Vec<Html>,
    },
    Text {
        /// Text which is escaped and re-indented when rendering
        text: String,
    },
    Raw {
        /// Raw text which is not escaped when rendering
        raw: String,
    },
    Seq {
        items: Vec<Html>,
    },
}

#[derive(Clone, Debug)]
pub struct Html {
    kind: Arc<HtmlKind>,
}

impl Html {
    pub fn elem(name: String, attrs: Vec<(String, String)>, content: Vec<Html>) -> Html {
        Html {
            kind: Arc::new(HtmlKind::Elem {
                name,
                attrs,
                content,
            }),
        }
    }

    pub fn text(text: String) -> Html {
        Html {
            kind: Arc::new(HtmlKind::Text { text }),
        }
    }

    pub fn raw(raw: String) -> Html {
        Html {
            kind: Arc::new(HtmlKind::Raw { raw }),
        }
    }

    pub fn seq(items: Vec<Html>) -> Html {
        Html {
            kind: Arc::new(HtmlKind::Seq { items }),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum PendingWhitespace {
    Space,
    Newline { indent: usize },
}

#[derive(Clone, Debug)]
struct Out {
    buf: String,
    pending_whitespace: Option<PendingWhitespace>,
}

impl Out {
    fn flush_whitespace(&mut self) {
        if let Some(whitespace) = self.pending_whitespace {
            match whitespace {
                PendingWhitespace::Space => self.buf.push(' '),
                PendingWhitespace::Newline { indent } => {
                    self.buf.push('\n');
                    self.buf.extend(std::iter::repeat(' ').take(indent));
                }
            }
            self.pending_whitespace = None;
        }
    }

    fn push(&mut self, c: char) {
        self.flush_whitespace();
        self.buf.push(c);
    }

    fn push_str(&mut self, s: &str) {
        self.flush_whitespace();
        self.buf.push_str(s);
    }

    fn newline(&mut self, indent: usize) {
        self.pending_whitespace = Some(PendingWhitespace::Newline { indent });
    }

    fn space(&mut self) {
        self.pending_whitespace = Some(PendingWhitespace::Space);
    }

    fn cancel_whitespace(&mut self) {
        self.pending_whitespace = None;
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FormatOptions {
    NoIndent,
    Indent { tab_width: usize },
}

impl std::default::Default for FormatOptions {
    fn default() -> Self {
        FormatOptions::NoIndent
    }
}

#[derive(Clone, Copy, Debug)]
enum RenderContext {
    NoIndent,
    Indent {
        tab_width: usize,
        indent_level: usize,
    },
}

// https://stackoverflow.com/questions/7381974/which-characters-need-to-be-escaped-in-html
fn escape_char(out: &mut Out, c: char) {
    match c {
        '&' => out.push_str("&amp;"),
        '<' => out.push_str("&lt;"),
        '>' => out.push_str("&gt;"),
        '"' => out.push_str("&quot;"),
        '\'' => out.push_str("&#39;"),
        _ => out.push(c),
    }
}

fn newline(out: &mut Out, ctx: &RenderContext) {
    match ctx {
        RenderContext::NoIndent => {
            out.cancel_whitespace();
        }
        RenderContext::Indent {
            tab_width,
            indent_level,
        } => {
            out.newline(*tab_width * *indent_level);
        }
    }
}

impl RenderContext {
    fn indent(&self) -> Self {
        match self {
            RenderContext::NoIndent => RenderContext::NoIndent,
            RenderContext::Indent {
                tab_width,
                indent_level,
            } => RenderContext::Indent {
                tab_width: *tab_width,
                indent_level: indent_level + 1,
            },
        }
    }
}

impl Html {
    fn render_rec(&self, out: &mut Out, ctx: &RenderContext) {
        match &*self.kind {
            HtmlKind::Elem {
                name,
                attrs,
                content,
            } => {
                newline(out, ctx);
                out.push('<');
                out.push_str(name);
                for (key, val) in attrs {
                    out.push(' ');
                    out.push_str(key);
                    out.push_str("=\"");
                    for c in val.chars() {
                        escape_char(out, c);
                    }
                    out.push('"');
                }
                out.push('>');

                if VOID_ELEMS.binary_search(&name.as_str()).is_ok() {
                    // TODO: Emit a warning here?
                    return;
                } else {
                    let sub_ctx = ctx.indent();
                    newline(out, &sub_ctx);
                    for item in content {
                        item.render_rec(out, &sub_ctx);
                    }
                    newline(out, ctx);
                    out.push_str("</");
                    out.push_str(name);
                    out.push('>');
                }
                newline(out, ctx);
            }
            HtmlKind::Text { text } => {
                for word in text.split_ascii_whitespace() {
                    for c in word.chars() {
                        escape_char(out, c);
                    }
                    out.space();
                }
            }
            HtmlKind::Raw { raw } => {
                out.push_str(raw);
            }
            HtmlKind::Seq { items } => {
                for item in items {
                    item.render_rec(out, ctx);
                }
            }
        }
    }

    pub fn render(&self, options: FormatOptions) -> String {
        let mut out = Out {
            buf: String::new(),
            pending_whitespace: None,
        };
        let ctx = match options {
            FormatOptions::NoIndent => RenderContext::NoIndent,
            FormatOptions::Indent { tab_width } => RenderContext::Indent {
                tab_width,
                indent_level: 0,
            },
        };
        self.render_rec(&mut out, &ctx);
        out.buf
    }
}

impl UserData for Html {}

impl<'lua> FromLua<'lua> for Html {
    fn from_lua(value: Value, _: &Lua) -> mlua::Result<Self> {
        match value {
            Value::String(s) => Ok(Html::text(s.to_str()?.to_owned())),
            Value::UserData(ud) => {
                let ud = ud.borrow::<Html>()?;
                Ok(ud.clone())
            }
            _ => Err(mlua::Error::FromLuaConversionError {
                from: value.type_name(),
                to: "Html",
                message: Some("expected either a string or Html userdata".to_owned()),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Modules<'lua> {
    pub html: Table<'lua>,
    pub el: Table<'lua>,
}

impl<'lua> Modules<'lua> {
    pub fn install(&self, lua: &Lua) -> mlua::Result<()> {
        let globals = lua.globals();
        globals.set("html", self.html.clone())?;
        globals.set("el", self.el.clone())?;
        Ok(())
    }
}

pub fn init_modules(lua: &Lua) -> mlua::Result<Modules> {
    let html_lib = lua.create_table()?;
    html_lib.set("to_html", lua.create_function(|_, html: Html| Ok(html))?)?;
    html_lib.set(
        "elem",
        lua.create_function(
            |_,
             (name, attrs, content): (String, Vec<[String; 2]>, Vec<Html>)|
             -> mlua::Result<Html> {
                Ok(Html::elem(
                    name,
                    attrs.into_iter().map(|[k, v]| (k, v)).collect(),
                    content,
                ))
            },
        )?,
    )?;
    html_lib.set(
        "text",
        lua.create_function(|_, text: String| -> mlua::Result<Html> { Ok(Html::text(text)) })?,
    )?;
    html_lib.set(
        "raw",
        lua.create_function(|_, text: String| -> mlua::Result<Html> { Ok(Html::raw(text)) })?,
    )?;
    html_lib.set(
        "seq",
        lua.create_function(|_, items: Vec<Html>| -> mlua::Result<Html> { Ok(Html::seq(items)) })?,
    )?;
    html_lib.set(
        "render",
        lua.create_function(
            |_, (html, options): (Html, Option<usize>)| -> mlua::Result<String> {
                let options = match options {
                    Some(tab_width) => FormatOptions::Indent { tab_width },
                    None => FormatOptions::NoIndent,
                };
                Ok(html.render(options))
            },
        )?,
    )?;

    let el_lib = lua
        .load(
            r#"
      local html = ...

      local el_mt = {}

      function el_mt:__index(key)
          if type(key) ~= "string" then
              return nil
          end
          return function(content)
              if type(content) == "string" then
                  return html.elem(key, {}, {html.text(content)})
              elseif type(content) == "table" then
                  local attrs = {}
                  for k, v in pairs(content) do
                      if type(k) == "string" then
                          table.insert(attrs, {k, v})
                      end
                  end
                  local children = {}
                  for _, c in ipairs(content) do
                      table.insert(children, c)
                  end
                  return html.elem(key, attrs, children)
              else
                  error("Argument to element function must be a string or table")
              end
          end
      end

      local el = {}
      setmetatable(el, el_mt)

      return el
      "#,
        )
        .call(html_lib.clone())?;

    Ok(Modules {
        html: html_lib,
        el: el_lib,
    })
}

pub fn install_modules(lua: &Lua) -> mlua::Result<()> {
    let modules = init_modules(lua)?;
    modules.install(lua)
}

pub fn create_lua() -> Lua {
    let lua = Lua::new();
    install_modules(&lua).unwrap();
    lua
}

pub fn render<'lua, 'a>(
    lua: &'lua Lua,
    options: FormatOptions,
    chunk: impl AsChunk<'lua, 'a>,
) -> mlua::Result<String> {
    let chunk = lua.load(chunk);
    let html: Html = chunk.call(())?;
    Ok(html.render(options))
}
