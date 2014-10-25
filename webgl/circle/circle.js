var gVertexShaderSrc = 
// Attributes (input to vertex shader)
"attribute vec4 a_position;\n" +
 // convert from model space to normalized device coordinate space
"uniform mat4 u_modelViewProjectionMatrix;\n" +
"varying vec2 uUV;\n" +
"void main() { \n" +
"  gl_Position = u_modelViewProjectionMatrix * a_position;\n" +
"  uUV = vec2(a_position);\n" +
"}";

// Make everything red.
var gFragmentShaderSrc =
"precision mediump float;\n" +
"uniform vec4 u_fillColor;\n" +
"varying vec2 uUV;\n" +
"void main() {\n" +
"  float d = 2.0*distance(uUV, vec2(0.0,0.0));\n" +
"  gl_FragColor = vec4((1.0-d)*vec3(u_fillColor), 1);\n" +
"}";

function circle(canvas) {
    draw(canvas);
}

function compileShader(gl, src, type) {
    var shader = gl.createShader(type);
    gl.shaderSource(shader, src);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error("Shader error: " + gl.getShaderInfoLog(shader));
    }
    return shader;
}

function draw(canvas) {
    var gl= canvas.getContext('webgl');
    gl.viewportWidth = canvas.width;
    gl.viewportHeight = canvas.height;

    var vertexShader = compileShader(gl, gVertexShaderSrc, gl.VERTEX_SHADER);
    var fragmentShader = compileShader(gl, gFragmentShaderSrc, gl.FRAGMENT_SHADER);
    var program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error("Error linking shaders. " + gl.getProgramInfoLog(program));
    }

    gl.useProgram(program);

    var aPosition = gl.getAttribLocation(program, "a_position");
    var uMVP = gl.getUniformLocation(program, "u_modelViewProjectionMatrix");
    var uFillColor = gl.getUniformLocation(program, "u_fillColor");

    var mvp = mat4.create();
    mat4.identity(mvp);
    gl.uniformMatrix4fv(uMVP, false, mvp);

    gl.uniform4f(uFillColor, 1, 0, 0, 1);

    gl.enableVertexAttribArray(aPosition);
    gl.clearColor(0,0,0,1);
    gl.viewport(0,0,canvas.width,canvas.height);
    gl.clear(gl.COLOR_BUFFER_BIT);

    var triangleBuf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, triangleBuf);
    var v = [
        -0.5, -0.5,
        -0.5, 0.5,
        0.5, -0.5,
        0.5, 0.5 ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(v), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aPosition, 2, gl.FLOAT, false, 0, 0);
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
}

