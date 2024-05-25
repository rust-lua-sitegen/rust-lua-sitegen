use mlua::{FromLua, Lua, Table, UserData, Value};

use crate::html::Html;

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
        "render_with_indent",
        lua.create_function(
            |_, (html, tab_width): (Html, usize)| -> mlua::Result<String> {
                Ok(html.render_with_indent(tab_width))
            },
        )?,
    )?;
    html_lib.set(
        "render_no_indent",
        lua.create_function(|_, html: Html| -> mlua::Result<String> {
            Ok(html.render_no_indent())
        })?,
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
