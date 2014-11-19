//
//  main.cpp
//  hello-opencl
//
//  Created by Douglas Richardson on 11/18/14.
//  Copyright (c) 2014 Doug Richardson. All rights reserved.
//

#include <cstdlib>
#include <iostream>
#include <vector>
#include <fstream>

#ifdef __APPLE__
#include <OpenCL/OpenCL.h>
#elif __linux__
#include <CL/cl.h>
#endif

template <typename T>
void ok(T rc) {
    if(rc != CL_SUCCESS) {
        std::cerr << "OpenCL function failed with " << rc << std::endl;
        std::abort();
    }
}

template <>
void ok(bool rc) {
    if(!rc) {
        std::cerr << "Failed with false" << std::endl;
        std::abort();
    }
}

template <typename Container>
void print_elements(Container const & v, std::ostream & out = std::cout) {
    out << "{";
    auto itr = v.begin();
    if (itr != v.end()) {
        out << *itr;
        ++itr;
    }
    for(; itr != v.end(); ++itr) {
        out << " " << *itr;
    }
    out << "}";
}

static void
create_context_error_callback(const char *s, const void *, size_t, void *)
{
    std::cerr << "create context error: " << s << std::endl;
}

struct StringResult {
    bool ok;
    std::string contents;
};

StringResult read_file_contents(char const* filename) {
    std::ifstream in(filename, std::ios::in | std::ios::binary);
    if (in) {
        std::string contents;
        in.seekg(0, std::ios::end);
        contents.resize(in.tellg());
        in.seekg(0, std::ios::beg);
        in.read(&contents[0], contents.size());
        if (in.gcount() != static_cast<std::streamsize>(contents.size())) {
            return {false, ""};
        }
        return {true, contents};
    }
    return {false, ""};
}

int main(int argc, const char * argv[]) {
    using std::vector;
    using std::endl;
    
    std::ostream & out = std::cout;
    
    //
    // Platform info
    //
    cl_uint platform_count = 0;
    auto rc = ::clGetPlatformIDs(0, nullptr, &platform_count);
    ok(rc);
    vector<cl_platform_id> platforms(platform_count);
    rc = ::clGetPlatformIDs(platform_count, &platforms[0], nullptr);
    ok(rc);
    out << "platforms: "; print_elements(platforms); out << '\n';
    for(auto platform : platforms) {
        char name[100], version[100];
        rc = ::clGetPlatformInfo(platform, CL_PLATFORM_NAME, sizeof(name), &name, nullptr);
        ok(rc);
        rc = ::clGetPlatformInfo(platform, CL_PLATFORM_VERSION, sizeof(version), &version, nullptr);
        ok(rc);
        out << "platform " << platform << " name is " << name << ", version is " << version << endl;
    }
    
    if (platforms.size() <= 0) {
        out << "ERROR: no platforms found.";
        std::exit(1);
    }
    
    auto const platform = platforms[0];
    
    //
    // Devices
    //
    cl_uint device_count = 0;
    rc = ::clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 0, nullptr, &device_count);
    ok(rc);
    vector<cl_device_id> devices(device_count);
    rc = ::clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, device_count, &devices[0], nullptr);
    ok(rc);
    out << "GPU devices: "; print_elements(devices); out << '\n';
    for(auto device : devices) {
        char buf[100];
        rc = ::clGetDeviceInfo(device, CL_DEVICE_NAME, sizeof(buf), buf, nullptr);
        ok(rc);
        out << "device " << device << " name is " << buf << '\n';
    }
    
    //
    // Context
    //
    auto context = ::clCreateContext(nullptr, device_count, &devices[0], create_context_error_callback, nullptr, &rc);
    
    ok(rc);
    out << "context: " << context << '\n';
    
    //
    // Create program
    //
    auto result = read_file_contents("hello.cl");
    if(!result.ok) {
        std::cerr << "Failed to read hello.cl" << "\n";
        std::exit(1);
    }
    ok(result.ok);
    out << "contents are: " << result.contents << "\n";
    
    char const* source = &result.contents[0];
    auto source_len = result.contents.size();
    auto program = ::clCreateProgramWithSource(context, 1, &source, &source_len, &rc);
    ok(rc);
    // clBuildProgram can run asynchronously, if the callback is provided.
    char const* build_options = "-cl-strict-aliasing"; // 
    out << "program created. building..." << std::flush;
    ::clBuildProgram(program, 0, nullptr, build_options, nullptr, nullptr);
    out << "done\n";

    //
    // Create kernel
    //
    auto kernel = ::clCreateKernel(program, "square", &rc);
    ok(rc);
    out << "kernel created\n";

    constexpr size_t count = 4000;
    auto in_buf = ::clCreateBuffer(context,
            CL_MEM_READ_WRITE,
            sizeof(float)*count,
            nullptr,
            &rc);
    ok(rc);
    auto out_buf = ::clCreateBuffer(context,
            CL_MEM_READ_WRITE,
            sizeof(float)*count,
            nullptr,
            &rc);
    ok(rc);
    out << "buffers created\n";

    //
    // Set arguments
    //
    rc = ::clSetKernelArg(kernel, 0, sizeof(in_buf), &in_buf);
    ok(rc);
    rc = ::clSetKernelArg(kernel, 1, sizeof(out_buf), &out_buf);
    ok(rc);
    rc = ::clSetKernelArg(kernel, 2, sizeof(count), &count);
    ok(rc);

    out << "arguments set\n";

    //
    // Create a command queue
    //
    auto command_queue = ::clCreateCommandQueue(context, devices[0], 0, &rc);
    ok(rc);
    out << "command queue created\n";

    //
    // Set input values
    //
    float in_values[count];
    for(size_t i = 0; i < count; ++i) {
        in_values[i] = static_cast<float>(i);
    }
    cl_event write_event;
    rc = ::clEnqueueWriteBuffer(command_queue, in_buf, CL_FALSE, 0, sizeof(float)*count, in_values, 0, nullptr, &write_event);
    ok(rc);
    out << "input values written\n"; 

    //
    // Execute kernel
    //
    cl_event task_event;
    rc = ::clEnqueueTask(command_queue, kernel, 1, &write_event, &task_event);
    ok(rc);
    out << "task enqueued" << endl;

    //
    // Read output values
    //
    float out_values[count];
    out << "out_values size " << sizeof(out_values) << ", other " << sizeof(float)*count << endl;
    cl_event read_event;
    rc = ::clEnqueueReadBuffer(command_queue, out_buf, CL_FALSE, 0, sizeof(float)*count, out_values, 1, &task_event, &read_event);
    ok(rc);
    
    rc = ::clWaitForEvents(1, &read_event);
    ok(rc);
    out << "task complete\n";

    out << "Read output values" << endl;
    for(size_t i = 0; i < count; ++i) {
        out << "out_values[" << i << "]=" << out_values[i] << endl;
    }
        
    // all done
    ::clReleaseCommandQueue(command_queue);
    ::clReleaseMemObject(out_buf);
    ::clReleaseMemObject(in_buf);
    ::clReleaseKernel(kernel);
    ::clReleaseProgram(program);
    ::clReleaseContext(context);
    return 0;
}
