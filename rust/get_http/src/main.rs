fn main() -> Result<(), Box<dyn std::error::Error>> {
    let body = reqwest::blocking::get("https://dougrichardson.us/")?.text()?;

    println!("BODY: {:#?}", body);

    Ok(())
}
