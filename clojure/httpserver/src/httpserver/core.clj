(ns httpserver.core)

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "Hello World"})

(use 'ring.adapter.jetty)
(run-jetty handler {:port 3000})
