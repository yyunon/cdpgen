use std::fs;
use std::io::{Error, Write};
use std::path::PathBuf;

pub(crate) const FILE_HEADER: &str = "// [BEGIN] Generated by pdl gen\n";
pub(crate) const FILE_FOOTER: &str = "\n// [END] Generated by pdl gen\n";

pub(crate) const SOURCE_DIR: &str = "src";

pub(crate) const CARGO_TOML: &str = r#"[package]
name = "rscdp"
version = "0.1.0"
edition = "2021"

[dependencies]
anyhow = "1.0.95"
serde = { version = "1.0.217", features = ["derive"] }
serde_json = "1.0.135"
"#;

pub(crate) const UTILS_RS: &str = r##"use anyhow::{Result, *};
use serde::*;

#[derive(Debug, Deserialize, Serialize)]
pub struct Empty {}

pub struct CDP<T> {
    pub request: Option<String>,
    pub response: Option<String>,
    pub response_parser: fn(String) -> T,
}

impl<T> CDP<T>
where
    T: serde::de::DeserializeOwned,
{
    pub fn new(parser_func: fn(String) -> T) -> Self {
        Self {
            request: None,
            response: None,
            response_parser: parser_func,
        }
    }

    pub fn set_request(&mut self, request: String) {
        self.request = Some(request);
    }

    // Return the parsed request message in json val
    pub fn request(&self) -> Result<serde_json::Value> {
        match self.request.as_ref() {
            Some(req) => Ok(serde_json::from_str(req.as_str())?),
            None => Err(anyhow!("Request field is not set yet!")),
        }
    }

    // Return the parsed response message in class
    pub fn response(&self, response: String) -> Result<T> {
        Ok((self.response_parser)(response))
    }
}

/*
Some example functions

/*
Represents deep serialized value.
*/
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct DeepSerializedValue {
    #[serde(rename = "type")]
    pub _type: String,
    #[serde(rename = "value")]
    pub value: String,
    #[serde(rename = "objectId")]
    pub objectId: String,
    /*
    Set if value reference met more then once during serialization. In such
    case, value is provided only to one of the serialized values. Unique
    per value in the scope of one CDP call.
    */
    #[serde(rename = "weakLocalObjectReference")]
    pub weakLocalObjectReference: String,
}

/*
Represents options for serialization. Overrides `generatePreview` and `returnByValue`.
*/
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct SerializationOptions {
    #[serde(rename = "serialization")]
    pub serialization: String,
    /*
    Deep serialization depth. Default is full depth. Respected only in `deep` serialization mode.
    */
    #[serde(rename = "maxDepth")]
    pub maxDepth: String,
    /*
    Embedder-specific parameters. For example if connected to V8 in Chrome these control DOM
    serialization via `maxNodeDepth: integer` and `includeShadowTree: "none" | "open" | "all"`.
    Values can be only of type string or integer.
    */
    #[serde(rename = "additionalParameters")]
    pub additionalParameters: String,
}

pub(crate) fn a_function() -> CDPParser<SerializationOptions> {
    pub fn response_parser(response: String) -> SerializationOptions {
        serde_json::from_str(response.as_str()).unwrap()
    }
    let mut request_handler = CDPParser::new(response_parser);
    let request = r#"{"name":"a_func" ,"method":"Runtime.enable"}"#.to_string();
    request_handler.set_request(request);
    request_handler
}
pub(crate) fn non_function() -> CDPParser<Empty> {
    pub fn response_parser(response: String) -> Empty {
        serde_json::from_str(response.as_str()).unwrap()
    }
    let mut request_handler = CDPParser::new(response_parser);
    let request = r#"{"name":"non_func" ,"method":"Runtime.disable"}"#.to_string();
    request_handler.set_request(request);
    request_handler
}

pub(crate) fn two_function() -> CDPParser<(SerializationOptions, DeepSerializedValue)> {
    pub fn response_parser(response: String) -> (SerializationOptions, DeepSerializedValue) {
        let js_val: serde_json::Value = serde_json::from_str(response.as_str()).unwrap();
        (
            serde_json::from_value(js_val["i"].clone()).unwrap(),
            serde_json::from_value(js_val["k"].clone()).unwrap(),
        )
    }
    let mut request_handler = CDPParser::new(response_parser);
    let request = r#"{"name":"non_func" ,"method":"Runtime.disable"}"#.to_string();
    request_handler.set_request(request);
    request_handler
}

pub(crate) fn two_function() -> CDPParser<(SerializationOptions, Option<DeepSerializedValue>)> {
    pub fn response_parser(
        response: String,
    ) -> (SerializationOptions, Option<DeepSerializedValue>) {
        let js_val: serde_json::Value = serde_json::from_str(response.as_str()).unwrap();
        (
            serde_json::from_value(js_val["i"].clone()).unwrap(),
            match js_val["s"] {
                serde_json::Value::Null => None,
                _ => Some(serde_json::from_value(js_val["s"].clone()).unwrap()),
            },
        )
    }
    let mut request_handler = CDPParser::new(response_parser);
    let request = r#"{"name":"non_func" ,"method":"Runtime.disable"}"#.to_string();
    request_handler.set_request(request);
    request_handler
}

*/
"##;

pub(crate) fn indent(num_tabs: usize, cmd: String) -> Result<String, std::io::Error> {
    let mut code = String::new();
    for line in cmd.lines() {
        let indent = "\t".repeat(num_tabs);
        code.push_str((indent + line + "\n").as_str());
    }

    Ok(code)
}
pub(crate) fn generate_command(command: &str) -> Result<String, std::io::Error> {
    Ok("/*\n".to_owned() + command + "\n*/\n")
}

pub(crate) fn generate_string_literal(command: &str) -> Result<String, std::io::Error> {
    Ok("\"".to_owned() + command + "\"")
}

pub(crate) fn generate_import(package_name: &str) -> Result<String, std::io::Error> {
    Ok(format!("use {};", package_name))
}

pub fn parse_json_file(file_name: PathBuf) -> Result<serde_json::Value, std::io::Error> {
    let file = fs::read_to_string(file_name).expect("Unable to read file");
    let json = serde_json::from_str(&file).expect("Unable to parse json");
    Ok(json)
}
pub fn write_to_file(file: &mut std::fs::File, text: String) -> Result<(), std::io::Error> {
    file.write_all((text + "\n").as_bytes())?;
    Ok(())
}

pub fn create_a_file(path: &PathBuf) -> Result<(), std::io::Error> {
    if !path.exists() {
        let mut file = fs::File::create(path)?;
    }
    Ok(())
}

pub fn generate_lib_rs(out_dir: &PathBuf, mods: &Vec<String>) -> Result<(), Error> {
    let mod_file = out_dir.join("lib.rs");
    create_a_file(&mod_file)?;

    let mut file = fs::OpenOptions::new().append(true).open(mod_file)?;
    write_to_file(&mut file, FILE_HEADER.to_owned())?;
    for domain_file in mods {
        write_to_file(&mut file, format!("pub mod {};", domain_file).to_owned())?;
    }
    write_to_file(&mut file, FILE_FOOTER.to_owned())?;
    Ok(())
}

pub fn generate_cargo_toml(out_dir: &PathBuf) -> Result<(), Error> {
    let cargo_file = out_dir.join("Cargo.toml");
    create_a_file(&cargo_file)?;

    let mut file = fs::OpenOptions::new().append(true).open(cargo_file)?;
    write_to_file(&mut file, CARGO_TOML.to_owned())?;
    Ok(())
}

pub fn generate_utils_rs(out_dir: &PathBuf) -> Result<(), Error> {
    let utils_file = out_dir.join("utils.rs");
    create_a_file(&utils_file)?;

    let mut file = fs::OpenOptions::new().append(true).open(utils_file)?;
    write_to_file(&mut file, UTILS_RS.to_owned())?;
    Ok(())
}

pub fn change_case_nth_letter(inp: &str, n: usize, case: bool) -> String {
    let mut out = String::new();
    for (i, c) in inp.chars().enumerate() {
        if i == n {
            if case {
                out.push_str(&c.to_uppercase().to_string());
            } else {
                out.push_str(&c.to_lowercase().to_string());
            }
        } else {
            out.push(c);
        }
    }
    out
}

pub fn insert_str_at_nth_line(inp: &mut str, insert: &str, n: usize) -> String {
    inp.lines()
        .enumerate()
        .map(|(i, line)| {
            if i == n {
                format!("{}\n{}", insert, line)
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}
