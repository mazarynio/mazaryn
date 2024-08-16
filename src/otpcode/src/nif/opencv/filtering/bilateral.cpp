#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to apply Bilateral Filter
static ERL_NIF_TERM bilateral_filter_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 5) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int d;
    double sigmaColor, sigmaSpace;
    if (!enif_get_int(env, argv[1], &d) ||
        !enif_get_double(env, argv[2], &sigmaColor) ||
        !enif_get_double(env, argv[3], &sigmaSpace)) {
        return enif_make_badarg(env);
    }

    int borderType;
    if (!enif_get_int(env, argv[4], &borderType)) {
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
    bilateralFilter(src, dst, d, sigmaColor, sigmaSpace, borderType);

    string outputPath = "bilateral_filtered_image.jpg";
    imwrite(outputPath, dst);

    return enif_make_string(env, outputPath.c_str(), ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 5, bilateral_filter_nif}
};

ERL_NIF_INIT(bilateral, nif_funcs, nullptr, nullptr, nullptr, nullptr)
