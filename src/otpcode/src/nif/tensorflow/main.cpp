#include <tensorflow/cc/client/client_session.h>
#include <tensorflow/cc/ops/standard_ops.h>
#include <tensorflow/core/framework/tensor.h>
#include <iostream>

int main() {
    using namespace tensorflow;
    using namespace tensorflow::ops;

    Scope root = Scope::NewRootScope();

    // Create a 2x2 constant matrix
    auto A = Const(root, {{1.f, 2.f}, {3.f, 4.f}});

    // Run the graph
    ClientSession session(root);
    std::vector<Tensor> outputs;
    TF_CHECK_OK(session.Run({A}, &outputs));

    // Print the result
    std::cout << outputs[0].matrix<float>() << std::endl;

    return 0;
}
