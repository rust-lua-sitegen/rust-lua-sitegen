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
        /// Text which is escaped when rendering
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

// https://stackoverflow.com/questions/7381974/which-characters-need-to-be-escaped-in-html
fn escape_char(out: &mut String, c: char) {
    match c {
        '&' => out.push_str("&amp;"),
        '<' => out.push_str("&lt;"),
        '>' => out.push_str("&gt;"),
        '"' => out.push_str("&quot;"),
        '\'' => out.push_str("&#39;"),
        _ => out.push(c),
    }
}

impl Html {
    fn render_rec(&self, out: &mut String) {
        match &*self.kind {
            HtmlKind::Elem {
                name,
                attrs,
                content,
            } => {
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
                    for item in content {
                        item.render_rec(out);
                    }
                    out.push_str("</");
                    out.push_str(name);
                    out.push('>');
                }
            }
            HtmlKind::Text { text } => {
                for c in text.chars() {
                    escape_char(out, c);
                }
            }
            HtmlKind::Raw { raw } => {
                out.push_str(raw);
            }
            HtmlKind::Seq { items } => {
                for item in items {
                    item.render_rec(out);
                }
            }
        }
    }

    pub fn render(&self) -> String {
        let mut out = String::new();
        self.render_rec(&mut out);
        out
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
        lua.create_function(|_, html: Html| -> mlua::Result<String> { Ok(html.render()) })?,
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
            local tag = key:gsub("_", "-")
            return function(content)
                if type(content) == "string" then
                    return html.elem(tag, {}, {html.text(content)})
                elseif type(content) == "table" then
                    local attrs = {}
                    for k, v in pairs(content) do
                        if type(k) == "string" then
                            table.insert(attrs, {k:gsub("_", "-"), v})
                        end
                    end
                    local children = {}
                    for _, c in ipairs(content) do
                        table.insert(children, c)
                    end
                    return html.elem(tag, attrs, children)
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

pub fn render<'lua, 'a>(lua: &'lua Lua, chunk: impl AsChunk<'lua, 'a>) -> mlua::Result<String> {
    let chunk = lua.load(chunk);
    let html: Html = chunk.call(())?;
    Ok(html.render())
}
