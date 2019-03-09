//! HTML.

#![allow(dead_code)]

use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Display;
use std::iter::FromIterator;

pub trait Node: Display {}

pub(crate) struct Element {
    tag: String,
    attrs: BTreeMap<String, String>,
    children: Vec<Box<dyn Node>>,
}

struct VoidElement {
    tag: String,
    attrs: BTreeMap<String, String>,
}

pub(crate) struct TextNode {
    text: String,
}

impl fmt::Display for Element {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}", self.tag)?;
        for attr in self.attrs.iter() {
            write!(f, " {}={:?}", attr.0, attr.1)?;
        }
        write!(f, ">")?;
        for node in &self.children {
            write!(f, "{}", node)?;
        }
        write!(f, "</{}>", self.tag)?;
        Ok(())
    }
}

impl Node for Element {}

impl Element {
    pub(crate) fn new<S, A, C>(tag: S, attrs: A, children: C) -> Self
    where
        S: Into<String>,
        A: IntoIterator<Item = (String, String)>,
        C: IntoIterator<Item = Box<dyn Node>>,
    {
        Element {
            tag: tag.into(),
            attrs: BTreeMap::from_iter(attrs),
            children: Vec::from_iter(children),
        }
    }
}

#[allow(unused_macros)]
macro_rules! attrs {
    ($(($x:expr, $y:expr)),*) => {{
        vec![$( (String::from($x), String::from($y)), )*]
    }}
}

#[allow(unused_macros)]
macro_rules! children {
    ($($x:expr),*) => {{
        let v: Vec<Box<dyn Node>> = vec![$( Box::new($x), )*];
        v
    }}
}

impl fmt::Display for VoidElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}", self.tag)?;
        for attr in self.attrs.iter() {
            write!(f, " {}={:?}", attr.0, attr.1)?;
        }
        write!(f, ">")?;
        Ok(())
    }
}

impl Node for VoidElement {}

impl VoidElement {
    fn new<S, A>(tag: S, attrs: A) -> Self
    where
        S: Into<String>,
        A: IntoIterator<Item = (String, String)>,
    {
        VoidElement {
            tag: tag.into(),
            attrs: BTreeMap::from_iter(attrs),
        }
    }
}

impl fmt::Display for TextNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Node for TextNode {}

impl TextNode {
    pub(crate) fn new<S>(text: S) -> Self
    where
        S: Into<String>,
    {
        TextNode { text: text.into() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_node() {
        assert_eq!(Element::new("a", None, vec![]).to_string(), "<a></a>");
        assert_eq!(TextNode::new("a").to_string(), "a");
        assert_eq!(
            Element::new("a", None, children![TextNode::new("b")]).to_string(),
            "<a>b</a>"
        );
        assert_eq!(
            Element::new(
                "a",
                None,
                children![TextNode::new("b"), Element::new("span", None, vec![])]
            )
            .to_string(),
            "<a>b<span></span></a>"
        );

        assert_eq!(VoidElement::new("br", None).to_string(), "<br>");
        assert_eq!(
            VoidElement::new("br", attrs![("class", "x")]).to_string(),
            r#"<br class="x">"#
        );

        assert_eq!(
            Element::new(
                "div",
                attrs![("class", "y"), ("id", r#"It's an "applicative functor.""#)],
                children![
                    TextNode::new("hello"),
                    VoidElement::new("img", attrs![("src", "the image of stletiori.png")])
                ],
            )
            .to_string(),
            r#"<div class="y" id="It\'s an \"applicative functor.\"">hello<img src="the image of stletiori.png"></div>"#
        );
    }
}
