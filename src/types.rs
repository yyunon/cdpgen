use convert_case::{Case, Casing};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::Error;
use std::path::PathBuf;
use std::string::String;

use crate::generators::*;

pub(crate) fn generate_rs_ref(_ref: &str) -> String {
    let ref_path_vec: Vec<&str> = _ref.split(".").collect();
    match ref_path_vec.len() {
        1 => ref_path_vec[0].to_owned(),
        _ => {
            ref_path_vec[1..ref_path_vec.len()].join("::")
            //let ref_path_no_domain = ref_path_vec[1..ref_path_vec.len()].join("::");
            //let mut ref_path = ref_path_vec[0].to_lowercase().replace("\"", "");
            //ref_path.push_str("::");
            //ref_path.push_str(ref_path_no_domain.as_str());
            //ref_path
        }
    }
}
pub(crate) fn generate_dependency_for_ref(_ref: &str) -> Option<(String, String)> {
    let ref_path_vec: Vec<&str> = _ref.split(".").collect();
    match ref_path_vec.len() {
        1 => None,
        _ => Some((
            ref_path_vec[0].to_lowercase().replace("\"", ""),
            ref_path_vec[1..ref_path_vec.len()].join("::"),
        )),
    }
}
pub(crate) fn convert_to_rust_string(inp: String, case: Case) -> (String, String) {
    let inp_stripped = inp.replace("-", "_");
    match inp_stripped.as_str() {
        "type" => (inp, "CDPtype".to_string()),
        "$ref" => (inp, "CDPref".to_string()),
        "enum" => (inp, "CDPkenum".to_string()),
        "override" => (inp, "CDPoverride".to_string()),
        "Self" => (inp, "CDPSelf".to_string()),
        _ => (inp, inp_stripped.to_case(case)),
    }
}
pub(crate) fn convert_to_rust_type(js_type: &str) -> Result<String, std::io::Error> {
    match js_type {
        "any" | "string" => Ok("String".to_owned()),
        "number" => Ok("f64".to_owned()),
        "integer" => Ok("i64".to_owned()),
        "boolean" => Ok("bool".to_owned()),
        "array" => Ok("Vec<String>".to_owned()),
        "object" => Ok("serde_json::Value".to_owned()),
        _ => unimplemented!("Type not implemented"),
    }
}

pub trait CDPDependencyManager {
    fn generated_dependencies_iter(
        &self,
    ) -> std::collections::hash_map::Iter<String, HashSet<String>>;
}
macro_rules! IMPL_DEPENDENCY_TRAIT {
    (for $($t:ty),+) => {
        $(impl CDPDependencyManager for $t {
            fn generated_dependencies_iter(&self) -> std::collections::hash_map::Iter<String, HashSet<String>> {
                self.generated_dependencies.iter()
            }
        })*
    }
}

IMPL_DEPENDENCY_TRAIT!(for CDPCommand, CDPType, CDPEvent);

pub trait RustCodeGen {
    fn generate_rust_code(&mut self) -> Result<String, Error> {
        Ok("".to_string())
    }
    fn generate_rust_code_for_domain(&mut self, domain: String) -> Result<String, Error> {
        Ok("".to_string())
    }
}

#[derive(Debug, Deserialize)]
pub struct CDPItems {
    #[serde(rename = "type")]
    pub _type: Option<String>,
    #[serde(rename = "$ref")]
    pub _ref: Option<String>,
}

macro_rules! GENERATE_PROPERTY_STRUCT {
    ($($t:ident),+) => {
        $(
            #[derive(Debug, Deserialize)]
            pub struct $t {
            pub name: Option<String>,
            pub description: Option<String>,
            #[serde(rename = "type")]
            pub _type: Option<String>,
            #[serde(rename = "$ref")]
            pub _ref: Option<String>,
            #[serde(rename = "enum")]
            pub _enum: Option<Vec<String>>,
            pub items: Option<CDPItems>,
            pub optional: Option<bool>,
            pub experimental: Option<bool>,
            pub deprecated: Option<bool>,
            #[serde(default)]
            pub generated_dependencies: HashMap<String, HashSet<String>>,
        })*
    }
}
GENERATE_PROPERTY_STRUCT!(CDPProperty, CDPParameter, CDPReturn);

pub trait CDPPropertyTrait {
    fn name(&self) -> (String, String);
    fn items(&self) -> Option<&CDPItems>;
    fn description(&self) -> Option<&String>;
    fn _type(&self) -> String;
    fn _ref(&self) -> Option<&String>;
    fn generate_rust_type(&self) -> Result<String, Error> {
        let mut rust_type = String::from("String"); // Make string default
        if let Some(items) = self.items() {
            if let Some(_ref) = items._ref.as_ref() {
                rust_type = format!("Vec<{}>", generate_rs_ref(_ref.as_str()));
            } else if let Some(_type) = items._type.as_ref() {
                rust_type = format!("Vec<{}>", convert_to_rust_type(_type.as_str())?);
            }
        } else if let Some(_type) = self._ref().as_ref() {
            rust_type = generate_rs_ref(_type.as_str());
        }
        Ok(rust_type)
    }
    fn generate_dependency_if_exists(&self) -> Result<Option<(String, String)>, Error> {
        let mut dep = None;
        if let Some(items) = self.items() {
            if let Some(_ref) = items._ref.as_ref() {
                dep = generate_dependency_for_ref(_ref.as_str());
            }
        } else if let Some(_ref) = self._ref().as_ref() {
            dep = generate_dependency_for_ref(_ref.as_str());
        }
        Ok(dep)
    }
    fn generate_prop_declaration(&self) -> Result<String, Error> {
        let orig_prop_name = self.name().0;
        let prop_name = self.name().1;

        let prop_type = self.generate_rust_type()?;

        let mut prop_command = String::new();
        if let Some(description) = self.description() {
            prop_command = generate_command(description.as_str())?;
        }
        Ok(format!(
            "{}#[serde(rename = \"{}\")]\npub {}: {},\n",
            prop_command, orig_prop_name, prop_name, prop_type
        ))
    }
    fn generate_object_pair(&self) -> Result<String, Error> {
        let assign = format!(r##""{}": "##, self.name().0); // generate code to assign value to json

        let val = self.name().1.clone();
        let to_json_code = assign + val.as_str();
        Ok(to_json_code)
    }
    fn generate_to_json(&self, serde_value: &str) -> Result<String, Error> {
        let assign = format!("{}[{}] = ", serde_value, self.name().0); // generate code to assign value to json

        let mut val = String::from("");
        if let Some(items) = self.items() {
            if let Some(_ref) = items._ref.as_ref() {
                val = self.name().1.clone();
            } else if let Some(_type) = items._type.as_ref() {
                val = self.name().1.clone();
            }
        } else if let Some(_type) = self._ref().as_ref() {
            val = generate_rs_ref(_type.as_str());
        }
        let to_json_code = assign + format!("serde_json::to_string(&{})", val).as_str();
        Ok(to_json_code)
    }
}
macro_rules! IMPL_PROPERTY_TRAIT {
    (for $($t:ty),+) => {
        $(impl CDPPropertyTrait for $t {
            fn name(&self) -> (String, String) {
                match self.name.as_ref() {
                    None => ("Unknown".to_string(), "Unknown".to_string()),
                    Some(x) => convert_to_rust_string(x.clone(), Case::Snake),
                }
            }

            fn items(&self) -> Option<&CDPItems> {
                self.items.as_ref()
            }

            fn description(&self) -> Option<&String> {
                self.description.as_ref()
            }
            fn _type(&self) -> String {
                match self._type.as_ref() {
                    None => "Unknown".to_string(),
                    Some(x) => x.clone(),
                }
            }
            fn _ref(&self) -> Option<&String> {
                self._ref.as_ref()
            }
        })*
    }
}
IMPL_PROPERTY_TRAIT!(for CDPProperty, CDPParameter, CDPReturn);

#[derive(Debug, Deserialize)]
pub struct CDPType {
    pub id: Option<String>,
    pub description: Option<String>,
    #[serde(rename = "type")]
    pub _type: Option<String>,
    pub items: Option<CDPItems>,
    #[serde(rename = "enum")]
    pub _enum: Option<Vec<String>>,
    pub properties: Option<Vec<CDPProperty>>,
    #[serde(default)]
    pub generated_dependencies: HashMap<String, HashSet<String>>,
}

impl CDPType {
    fn generate_rust_struct(&mut self) -> Result<String, Error> {
        let mut code = String::new();
        let mut id = String::from("Unknown");
        if let Some(_id) = self.id.as_ref() {
            id = _id.clone();
        }
        if let Some(description) = self.description.as_ref() {
            code.push_str(generate_command(description.as_str()).unwrap().as_str());
        }
        code.push_str("#[derive(Debug, Deserialize, Serialize)]\n");
        code.push_str(format!("pub(crate) struct {} {{\n", id).as_str());
        for prop in self.properties.as_ref().unwrap() {
            let prop_code = prop.generate_prop_declaration()?;
            if let Some(dep) = prop.generate_dependency_if_exists()? {
                match self.generated_dependencies.get_mut(&dep.0) {
                    Some(v) => {
                        v.insert(dep.1);
                    }
                    None => {
                        let mut set = HashSet::new();
                        set.insert(dep.1);
                        self.generated_dependencies.insert(dep.0, set);
                    }
                }
            }
            code.push_str(indent(1, prop_code).unwrap().as_str());
        }
        code.push_str("}\n");
        Ok(code)
    }
    fn generate_rust_enum(&self) -> Result<String, Error> {
        let mut code = String::new();
        let mut id = String::from("Unknown");
        if let Some(_id) = self.id.as_ref() {
            id = _id.clone();
        }
        if let Some(description) = self.description.as_ref() {
            code.push_str(generate_command(description.as_str()).unwrap().as_str());
        }
        code.push_str("#[derive(Debug, Deserialize, Serialize)]\n");
        code.push_str(format!("pub(crate) enum {} {{\n", id).as_str());
        for item in self._enum.as_ref().unwrap() {
            code.push_str(
                indent(
                    1,
                    format!(
                        "#[serde(rename = \"{}\")]\n{},\n",
                        item,
                        convert_to_rust_string(item.clone(), Case::UpperCamel)
                            .1
                            .as_str()
                            .to_case(Case::UpperCamel),
                    ),
                )
                .unwrap()
                .as_str(),
            );
        }
        code.push_str("}\n");
        Ok(code)
    }
    fn generate_rust_primitive_type(&self) -> Result<String, Error> {
        let mut code = String::new();
        let mut id = String::from("Unknown");
        if let Some(_id) = self.id.as_ref() {
            id = _id.clone();
        }
        let mut js_type = String::from("string"); // Make string default
        if let Some(items) = self.items.as_ref() {
            if let Some(_ref) = items._ref.as_ref() {
                js_type = generate_rs_ref(_ref.as_str());
            } else if let Some(_type) = items._type.as_ref() {
                js_type = convert_to_rust_type(_type.as_str())?;
            }
        } else if let Some(_type) = self._type.as_ref() {
            js_type = convert_to_rust_type(_type.as_str())?;
        }
        if let Some(description) = self.description.as_ref() {
            code.push_str(generate_command(description.as_str()).unwrap().as_str());
        }
        code.push_str("#[derive(Debug, Deserialize, Serialize)]\n");
        code.push_str(format!("pub(crate) struct {}({});\n", id, js_type).as_str());
        Ok(code)
    }
}

impl RustCodeGen for CDPType {
    fn generate_rust_code(&mut self) -> Result<String, Error> {
        if self._enum.is_some() {
            self.generate_rust_enum()
        } else if self.properties.is_some() {
            self.generate_rust_struct()
        } else {
            self.generate_rust_primitive_type()
        }
    }
}

impl CDPParameter {
    pub(crate) fn generate_json(&self) -> String {
        let mut code = String::new();
        let (_, name) = self.name();
        code.push_str(format!("json_body[\"{}\"] = {};\n", name, name).as_str());
        code
    }
    pub(crate) fn generate_parameter_declaration(&self) -> Result<String, Error> {
        let mut code = String::new();
        let mut name_type = "".to_string();
        let (_, name) = self.name();
        if let Some(items) = self.items.as_ref() {
            if let Some(_ref) = items._ref.as_ref() {
                name_type.push_str(format!("Vec<{}>", generate_rs_ref(_ref.as_str())).as_str());
            } else if let Some(_type) = items._type.as_ref() {
                name_type
                    .push_str(format!("Vec<{}>", convert_to_rust_type(_type.as_str())?).as_str());
            }
        } else if let Some(_ref) = self._ref.as_ref() {
            name_type.push_str(generate_rs_ref(_ref.as_str()).as_str());
        } else {
            name_type.push_str("String");
        }

        code = format!("{}: {}", name.clone(), name_type.clone());

        if let Some(optional) = self.optional {
            if optional {
                code = format!("{}: Option<{}>", name, name_type);
            }
        }
        Ok(code)
    }
}

impl CDPReturn {
    fn name(&self) -> (String, String) {
        match self.name.as_ref() {
            None => ("Unknown".to_string(), "Unknown".to_string()),
            Some(x) => convert_to_rust_string(x.clone(), Case::Snake),
        }
    }
    pub(crate) fn generate_return_declaration(&self) -> Result<String, Error> {
        let mut code = String::new();
        let mut name_type = "".to_string();
        if let Some(items) = self.items.as_ref() {
            if let Some(_ref) = items._ref.as_ref() {
                name_type.push_str(format!("Vec<{}>", generate_rs_ref(_ref.as_str())).as_str());
            } else if let Some(_type) = items._type.as_ref() {
                name_type
                    .push_str(format!("Vec<{}>", convert_to_rust_type(_type.as_str())?).as_str());
            }
        } else if let Some(_ref) = self._ref.as_ref() {
            name_type.push_str(generate_rs_ref(_ref).as_str());
        } else {
            name_type.push_str("String");
        }

        code = name_type.clone();

        if let Some(optional) = self.optional {
            if optional {
                code = format!("Option<{}>", name_type);
            }
        }
        Ok(code)
    }
    pub(crate) fn generate_return(&self) -> Result<String, Error> {
        let mut code = String::new();
        let name_code = format!("js_val[\"{}\"]", self.name().0);

        if let Some(optional) = self.optional {
            if optional {
                code = format!("match {} {{", name_code);
                code.push_str("\n\t\tserde_json::Value::Null => None,\n");
                code.push_str("\t\t_ => Some(\n");
                code.push_str(
                    format!(
                        "\t\t\tserde_json::from_value({}.clone()).unwrap()\n\t\t),\n",
                        name_code
                    )
                    .as_str(),
                );
                code.push_str("\t}\n");
            }
        } else {
            code = format!("serde_json::from_value({}.clone()).unwrap()", name_code);
        }
        Ok(code)
    }
}

#[derive(Debug, Deserialize)]
pub struct CDPCommand {
    pub name: String,
    pub description: Option<String>,
    pub experimental: Option<bool>,
    pub deprecated: Option<bool>,
    pub parameters: Option<Vec<CDPParameter>>,
    pub returns: Option<Vec<CDPReturn>>,
    #[serde(default)]
    pub generated_dependencies: HashMap<String, HashSet<String>>,
}

impl CDPCommand {
    fn name(&self) -> (String, String) {
        convert_to_rust_string(self.name.clone(), Case::Snake)
    }
}
impl RustCodeGen for CDPCommand {
    fn generate_rust_code_for_domain(&mut self, domain: String) -> Result<String, Error> {
        let mut code = String::new();

        // Generate function signature
        code.push_str(format!("pub(crate) fn {}(", self.name().1).as_str());

        // Generate parameters
        if let Some(parameters) = &self.parameters {
            let mut params_vec = vec![];
            for param in parameters.iter() {
                params_vec.push(param.generate_parameter_declaration()?);
                if let Some(dep) = param.generate_dependency_if_exists()? {
                    match self.generated_dependencies.get_mut(&dep.0) {
                        Some(v) => {
                            v.insert(dep.1);
                        }
                        None => {
                            let mut set = HashSet::new();
                            set.insert(dep.1);
                            self.generated_dependencies.insert(dep.0, set);
                        }
                    }
                }
            }
            code.push_str("\n\t");
            code.push_str(params_vec.join(",\n\t").as_str());
        }

        // Generate return type
        let mut return_type = String::from("serde_json::Value"); // Default return type
        let cdp_parser_wrapper = String::from("CDPParser<");
        let json_return = String::from("");
        if let Some(returns) = &self.returns {
            match returns.len() {
                0 => return_type = json_return + "Empty",
                1 => {
                    return_type = {
                        if let Some(dep) = returns[0].generate_dependency_if_exists()? {
                            match self.generated_dependencies.get_mut(&dep.0) {
                                Some(v) => {
                                    v.insert(dep.1);
                                }
                                None => {
                                    let mut set = HashSet::new();
                                    set.insert(dep.1);
                                    self.generated_dependencies.insert(dep.0, set);
                                }
                            }
                        }
                        json_return + returns[0].generate_return_declaration()?.as_str()
                    }
                }
                _ => {
                    return_type = json_return + "(";
                    let mut returns_vec = vec![];
                    for ret in returns.iter() {
                        returns_vec.push(ret.generate_return_declaration()?);
                        if let Some(dep) = ret.generate_dependency_if_exists()? {
                            match self.generated_dependencies.get_mut(&dep.0) {
                                Some(v) => {
                                    v.insert(dep.1);
                                }
                                None => {
                                    let mut set = HashSet::new();
                                    set.insert(dep.1);
                                    self.generated_dependencies.insert(dep.0, set);
                                }
                            }
                        }
                    }
                    return_type.push_str(returns_vec.join(", ").as_str());
                    return_type.push_str(") ");
                }
            }
        }
        code.push_str(format!(") -> {}{}> {{\n", cdp_parser_wrapper, return_type).as_str());

        // Generate function body (Description)
        if let Some(description) = self.description.as_ref() {
            code.push_str(indent(1, generate_command(description.as_str()).unwrap())?.as_str());
        }

        let mut json_body_main = String::from("let command = json!({\n");
        json_body_main.push_str(
            indent(
                1,
                format!("\"method\": \"{}.{}\",\n", domain.clone(), self.name().0),
            )?
            .as_str(),
        );

        // Generate function body (Response Parser)
        code.push_str(
            indent(
                1,
                format!(
                    "pub fn response_parser(response: String) -> {} {{\n",
                    return_type,
                ),
            )
            .unwrap()
            .as_str(),
        );

        let mut parsed_json_string_result = String::from(
            "\t\tlet js_val: serde_json::Value = serde_json::from_str(response.as_str()).unwrap();\n",
        );
        let mut return_val = String::from("js_val");
        if let Some(returns) = &self.returns {
            match returns.len() {
                0 => return_val = "js_val".to_owned(),
                1 => return_val = returns[0].generate_return()?,
                _ => {
                    return_val = "(\n\t".to_owned();
                    let mut returns_vec = vec![];
                    for ret in returns.iter() {
                        returns_vec.push(ret.generate_return()?);
                    }
                    return_val.push_str(returns_vec.join(",\n\t").as_str());
                    return_val.push_str(")");
                }
            }
        }
        code.push_str(parsed_json_string_result.as_str());
        code.push_str(indent(2, format!("{}\n", return_val)).unwrap().as_str());
        code.push_str(indent(1, "}".to_owned()).unwrap().as_str());

        // Generate function body (Response Parser)
        code.push_str(
            indent(
                1,
                "let mut request_handler = CDPParser::new(response_parser);\n".to_owned(),
            )
            .unwrap()
            .as_str(),
        );
        // Generate function body (Parameters to JSON)
        if let Some(parameters) = self.parameters.as_ref() {
            let mut json_body_vec = vec![];
            for param in parameters.iter() {
                json_body_vec.push(param.generate_object_pair()?);
            }
            if json_body_vec.len() > 0 {
                json_body_main.push_str(
                    indent(1, String::from("\"params\" : {\n"))
                        .unwrap()
                        .as_str(),
                );
                json_body_main.push_str(indent(2, json_body_vec.join(",\n")).unwrap().as_str());
                json_body_main.push_str(indent(1, "}\n".to_owned()).unwrap().as_str());
            }
        }
        json_body_main.push_str("});\n");

        code.push_str(indent(1, json_body_main).unwrap().as_str());

        code.push_str(
            indent(
                1,
                "request_handler.set_request(command.to_string());\n".to_owned(),
            )
            .unwrap()
            .as_str(),
        );
        code.push_str(indent(1, "request_handler\n".to_owned()).unwrap().as_str());
        code.push_str("}\n");
        Ok(code)
    }
}

#[derive(Debug, Deserialize)]
pub struct CDPEvent {
    pub name: String,
    pub description: Option<String>,
    pub deprecated: Option<bool>,
    pub experimental: Option<bool>,
    pub parameters: Option<Vec<CDPParameter>>,
    pub domain: Option<String>,
    #[serde(default)]
    pub generated_dependencies: HashMap<String, HashSet<String>>,
}

impl CDPEvent {
    fn domain(&mut self, _domain: String) {
        self.domain = Some(_domain.clone());
    }
}

impl RustCodeGen for CDPEvent {
    fn generate_rust_code_for_domain(&mut self, domain: String) -> Result<String, Error> {
        let mut code = String::new();
        let names = self.name.split(".").collect::<Vec<&str>>();
        let name = change_case_nth_letter(names[names.len() - 1], 0, true);
        if let Some(description) = self.description.as_ref() {
            code.push_str(generate_command(description.as_str()).unwrap().as_str());
        }
        code.push_str("#[derive(Debug, Deserialize, Serialize)]\n");
        code.push_str(format!("pub(crate) struct {} {{\n", name).as_str());
        if let Some(parameters) = self.parameters.as_ref() {
            for prop in parameters {
                let prop_code = prop.generate_prop_declaration()?;
                if let Some(dep) = prop.generate_dependency_if_exists()? {
                    match self.generated_dependencies.get_mut(&dep.0) {
                        Some(v) => {
                            v.insert(dep.1);
                        }
                        None => {
                            let mut set = HashSet::new();
                            set.insert(dep.1);
                            self.generated_dependencies.insert(dep.0, set);
                        }
                    }
                }
                code.push_str(indent(1, prop_code).unwrap().as_str());
            }
        }
        code.push_str("}\n");
        Ok(code)
    }
}

#[derive(Debug, Deserialize)]
pub struct CDPDomain {
    pub domain: String,
    pub description: Option<String>,
    pub experimental: Option<bool>,
    pub dependencies: Option<Vec<String>>,
    pub types: Option<Vec<CDPType>>,
    pub commands: Vec<CDPCommand>,
    pub events: Option<Vec<CDPEvent>>,
    #[serde(default)]
    pub generated_dependencies: HashMap<String, HashSet<String>>,
}
impl CDPDomain {
    fn generate_code_for_part<T>(&mut self, part: &mut T) -> Result<String, Error>
    where
        T: CDPDependencyManager + RustCodeGen,
    {
        let code = part.generate_rust_code()?;
        part.generated_dependencies_iter().for_each(|(k, v)| {
            match self.generated_dependencies.get_mut(k) {
                Some(val) => {
                    val.extend(v.clone());
                }
                None => {
                    self.generated_dependencies.insert(k.clone(), v.clone());
                }
            }
        });
        Ok(code)
    }
}
impl RustCodeGen for CDPDomain {
    fn generate_rust_code(&mut self) -> Result<String, Error> {
        let mut code = String::new();
        code.push_str(FILE_HEADER);

        let mut generated_code = vec![];

        if self.types.is_some() || self.events.is_some() {
            // We do not use serde features in files with no struct or enum or primitive types
            generated_code.push(generate_import("serde::*")?);
        }
        generated_code.push(generate_import("serde_json::*")?);
        generated_code.push(generate_import("crate::utils::*")?);

        if let Some(types) = &mut self.types {
            for part in types.iter_mut() {
                //PATCH
                //TODO: Fix this later
                if part._enum.is_none()
                    && part.properties.is_none()
                    && self.domain.replace("\", \"", "").to_lowercase() == "fetch"
                {
                    if let Some(id) = part.id.as_ref() {
                        if id.to_case(Case::UpperCamel) == "RequestId" {
                            continue;
                        }
                    }
                }
                generated_code.push(part.generate_rust_code()?);
                part.generated_dependencies_iter().for_each(|(k, v)| {
                    match self.generated_dependencies.get_mut(k) {
                        Some(val) => {
                            val.extend(v.clone());
                        }
                        None => {
                            self.generated_dependencies.insert(k.clone(), v.clone());
                        }
                    }
                });
            }
        }

        for part in self.commands.iter_mut() {
            generated_code.push(part.generate_rust_code_for_domain(self.domain.clone())?);
            part.generated_dependencies_iter().for_each(|(k, v)| {
                match self.generated_dependencies.get_mut(k) {
                    Some(val) => {
                        val.extend(v.clone());
                    }
                    None => {
                        self.generated_dependencies.insert(k.clone(), v.clone());
                    }
                }
            });
        }

        if let Some(events) = &mut self.events {
            for part in events.iter_mut() {
                generated_code.push(part.generate_rust_code_for_domain(self.domain.clone())?);
                part.generated_dependencies_iter().for_each(|(k, v)| {
                    match self.generated_dependencies.get_mut(k) {
                        Some(val) => {
                            val.extend(v.clone());
                        }
                        None => {
                            self.generated_dependencies.insert(k.clone(), v.clone());
                        }
                    }
                });
            }
        }
        self.generated_dependencies
            .iter()
            .filter(|(k, _)| **k != self.domain.replace("\", \"", "").to_lowercase())
            .for_each(|(k, v)| {
                let modules = v.iter().map(String::from).collect::<Vec<String>>();
                match modules.len() {
                    1 => {
                        code.push_str(format!("use crate::{}::{};\n", k, modules[0]).as_str());
                    }
                    _ => {
                        code.push_str(
                            format!("use crate::{}::{{{}}};\n", k, modules.join(", ")).as_str(),
                        );
                    }
                }
            });
        code.push_str(generated_code.join("\n").as_str());
        code.push_str(FILE_FOOTER);
        Ok(code)
    }
}
