use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder};
use std::sync::Mutex;

#[get("/")]
async fn hello() -> impl Responder {
    HttpResponse::Ok().body("Hello world!")
}

#[post("/echo")]
async fn echo(req_body: String) -> impl Responder {
    HttpResponse::Ok().body(req_body)
}

async fn manual_hello() -> impl Responder {
    HttpResponse::Ok().body("Hey there!")
}

struct AppState {
    app_name: String,
}

#[get("/state/app_name")]
async fn get_app_name(data: web::Data<AppState>) -> String {
    let app_name = &data.app_name;
    format!("Hello {app_name}!")
}

struct AppStateWithCounter {
    counter: Mutex<i32>, // mutex used to mutate safely across threads
}

#[get("/state/increment")]
async fn increment(data: web::Data<AppStateWithCounter>) -> String {
    let mut counter = data.counter.lock().unwrap();
    *counter += 1;
    format!("Request number: {counter}")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    /*
    HttpServer accepts an application factory rather than an application instance. An
    HttpServer constructs an application instance for each thread. Therefore, application data
    must be constructed multiple times. If you want to share data between different threads, a
    shareable object should be used, e.g. Send + Sync.
    */

    let counter = web::Data::new(AppStateWithCounter {
        counter: Mutex::new(0),
    });

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(AppState {
                app_name: String::from("Actix Web"),
            }))
            .app_data(counter.clone())
            .service(hello)
            .service(echo)
            .route("/hey", web::get().to(manual_hello))
            .service(get_app_name)
            .service(increment)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
