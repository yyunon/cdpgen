use std::fs;
use std::io::{Error, Write};
use std::path::PathBuf;

pub mod generators;
pub mod types;

pub use generators::*;
pub use types::*;

fn main() -> Result<(), Error> {
    let devtools_dir: PathBuf = std::env::args()
        .nth(1)
        .expect("Please provide a valid path")
        .into();

    let out_dir: PathBuf = std::env::args()
        .nth(2)
        .expect("Please provide a valid output path")
        .into();

    if out_dir.exists() {
        fs::remove_dir_all(&out_dir)?;
    }
    fs::create_dir(&out_dir)?;
    fs::create_dir(&out_dir.join(SOURCE_DIR))?;

    let js_proto = devtools_dir.join("js_protocol.json");

    let browser_proto = devtools_dir.join("browser_protocol.json");

    if !browser_proto.exists() || !js_proto.exists() {
        panic!("Please provide a valid path that contains the browser_protocol.json and js_protocol.json files");
    }

    let js_proto_json = parse_json_file(js_proto)?;

    let browser_proto_json = parse_json_file(browser_proto)?;

    if js_proto_json["version"] != browser_proto_json["version"] {
        panic!("The versions of the browser_protocol.json and js_protocol.json files do not match");
    }

    let mut domain_files = vec![];

    let js_domains = js_proto_json["domains"].as_array().unwrap();

    let browser_domains = browser_proto_json["domains"].as_array().unwrap();

    for domain in js_domains.iter().chain(browser_domains.iter()) {
        let domain_name = domain["domain"]
            .to_string()
            .to_lowercase()
            .replace("\"", "");
        let domain_file_name = format!("{}/{}.rs", SOURCE_DIR, domain_name);
        let domain_file = out_dir.join(domain_file_name);
        println!("Parsing {}", domain["domain"]);
        let mut cdp_domain: CDPDomain = serde_json::from_value(domain.clone()).unwrap();
        create_a_file(&domain_file)?;
        write_to_file(
            &mut fs::OpenOptions::new().append(true).open(&domain_file)?,
            cdp_domain.generate_rust_code()?,
        )?;
        domain_files.push(domain_name);
    }

    domain_files.push("utils".to_owned());
    generate_utils_rs(&out_dir.join(SOURCE_DIR))?;

    generate_lib_rs(&out_dir.join(SOURCE_DIR), &domain_files)?;

    generate_cargo_toml(&out_dir)?;

    Ok(())
}
