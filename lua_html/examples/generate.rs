use lua_html;

fn main() {
    let lua = lua_html::create_lua();

    let result = lua_html::render(
        &lua,
        r#"
        return html.seq {
            html.raw "<!DOCTYPE html>",
            el.html {
                el.head {
                    el.title "Hello, World!",
                },
                el.body {
                    el.h1 "<<< Hello, World! >>>",
                    el.br {},
                    el.p "This is a paragraph.",
                },
            },
        }
    "#,
    )
    .unwrap();

    println!("{}", result);
}
