#include <iostream>
#include <vector>
#include "tensorflow/cc/client/client_session.h"
#include "tensorflow/cc/ops/standard_ops.h"
#include "tensorflow/core/framework/tensor.h"

int main() {
    using namespace tensorflow;
    using namespace tensorflow::ops;

    // Create a scope
    Scope root = Scope::NewRootScope();

    // Create constants in the computation graph
    auto A = Const(root, { {3.0, 2.0}, {1.0, 0.0} });
    auto B = Const(root, { {0.0, 1.0}, {2.0, 3.0} });

    // Matrix multiplication operation
    auto product = MatMul(root, A, B);

    // Create a session to run the graph
    ClientSession session(root);

    // Run the session to get the result
    std::vector<Tensor> outputs;
    TF_CHECK_OK(session.Run({product}, &outputs));

    // Print the result
    std::cout << "Result of matrix multiplication:\n" << outputs[0].matrix<float>() << std::endl;

    return 0;
}
