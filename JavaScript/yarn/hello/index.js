console.log("Hello, World!")

const http = require('http');
const port = 3000;
const hostname = 'localhost';

const server = http.createServer((req, res) => {
    console.log(`REQUEST: ${req.method} ${req.url}`);
    res.statusCode = 200;
    res.setHeader('Content-Type', 'text/plain');
    res.end('Hello, World!\n');
});

server.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
});


