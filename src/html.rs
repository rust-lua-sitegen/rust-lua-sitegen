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

    pub fn render_with_indent(&self, tab_width: usize) -> String {
        let mut out = Out {
            buf: String::new(),
            pending_whitespace: None,
        };
        let ctx = RenderContext::Indent {
            tab_width,
            indent_level: 0,
        };
        self.render_rec(&mut out, &ctx);
        out.buf
    }

    pub fn render_no_indent(&self) -> String {
        let mut out = Out {
            buf: String::new(),
            pending_whitespace: None,
        };
        let ctx = RenderContext::NoIndent;
        self.render_rec(&mut out, &ctx);
        out.buf
    }
}
