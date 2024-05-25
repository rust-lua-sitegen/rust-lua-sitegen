use lua_html;
use mlua::prelude::Lua;

fn main() {
    let lua = lua_html::lua::create_lua();

    let chunk = lua.load(
        r#"
        local h = el.p { "Hello, world! <test>", el.p { "hi" } }
        print(html.render_no_indent(h))
    "#,
    );

    chunk.exec().unwrap();
}
