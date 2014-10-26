'use strict';

var gVertexShaderSrc = 
// Attributes (input to vertex shader)
"attribute vec4 a_position;\n" +
 // convert from model space to normalized device coordinate space
"uniform mat4 u_modelToWorld;\n" +
"uniform mat4 u_worldToView;\n" +
"uniform mat4 u_viewToProjection;\n" +
"void main() { gl_Position = u_viewToProjection * u_worldToView * u_modelToWorld * a_position; }";

// Make everything red.
var gFragmentShaderSrc = "void main() { gl_FragColor = vec4(1,0,0,1); }";

function compileShader(gl, src, type) {
    var shader = gl.createShader(type);
    gl.shaderSource(shader, src);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error("Shader error: " + gl.getShaderInfoLog(shader));
    }
    return shader;
}

function draw() {
    var canvas = document.getElementById('mainCanvas');
    var data = JSON.parse(document.getElementById('data').value);

    var gl= canvas.getContext('webgl');
    gl.clearColor(0,0,0,1);
    gl.viewport(0,0,canvas.width,canvas.height);
    gl.clear(gl.COLOR_BUFFER_BIT);

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

    var uModelToWorld = gl.getUniformLocation(program, "u_modelToWorld");
    var uWorldToView = gl.getUniformLocation(program, "u_worldToView");
    var uViewToProjection = gl.getUniformLocation(program, "u_viewToProjection");
    
    gl.uniformMatrix4fv(uModelToWorld, false, new Float32Array(data.model));
    gl.uniformMatrix4fv(uWorldToView, false, new Float32Array(data.view));
    gl.uniformMatrix4fv(uViewToProjection, false, new Float32Array(data.projection));

    gl.enableVertexAttribArray(aPosition);
    
    var triangleBuf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, triangleBuf);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(data.verticies), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aPosition, 3, gl.FLOAT, false, 0, 0);

    var elementBuf = gl.createBuffer();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, elementBuf);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint8Array(data.indices), gl.STATIC_DRAW);
    gl.drawElements(gl.TRIANGLE_STRIP, data.indices.length, gl.UNSIGNED_BYTE, 0);

    gl.deleteBuffer(triangleBuf);
    gl.deleteBuffer(elementBuf);
}

