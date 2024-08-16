#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to apply dilation
static ERL_NIF_TERM dilate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 7) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int kernel_type, kernel_size, anchor_x, anchor_y, iterations, borderType;
    if (!enif_get_int(env, argv[1], &kernel_type) ||
        !enif_get_int(env, argv[2], &kernel_size) ||
        !enif_get_int(env, argv[3], &anchor_x) ||
        !enif_get_int(env, argv[4], &anchor_y) ||
        !enif_get_int(env, argv[5], &iterations) ||
        !enif_get_int(env, argv[6], &borderType)) {
        return enif_make_badarg(env);
    }

    cout << "Image path: " << imagePath << endl;
    if (!fs::exists(imagePath)) {
        cerr << "Error: File does not exist: " << imagePath << endl;
        return enif_make_string(env, "File does not exist", ERL_NIF_LATIN1);
    }

    Mat src = imread(imagePath, IMREAD_COLOR);
    if (src.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    Mat dst;
    Mat kernel = getStructuringElement(kernel_type, Size(kernel_size, kernel_size));
    Point anchor(anchor_x, anchor_y);

    dilate(src, dst, kernel, anchor, iterations, borderType);

    string outputPath = "dilated_image.jpg";
    imwrite(outputPath, dst);

    return enif_make_string(env, outputPath.c_str(), ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 7, dilate_nif}
};

ERL_NIF_INIT(dilate, nif_funcs, nullptr, nullptr, nullptr, nullptr)
