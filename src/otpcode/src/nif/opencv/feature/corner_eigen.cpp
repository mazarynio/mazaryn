#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to apply cornerEigenValsAndVecs
static ERL_NIF_TERM corner_eigenvals_and_vecs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 5) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    char outputPath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[1], outputPath, sizeof(outputPath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int blockSize, ksize, borderType;
    if (!enif_get_int(env, argv[2], &blockSize) ||
        !enif_get_int(env, argv[3], &ksize) ||
        !enif_get_int(env, argv[4], &borderType)) {
        return enif_make_badarg(env);
    }

    cout << "Image path: " << imagePath << endl;
    if (!fs::exists(imagePath)) {
        cerr << "Error: File does not exist: " << imagePath << endl;
        return enif_make_string(env, "File does not exist", ERL_NIF_LATIN1);
    }

    Mat src = imread(imagePath, IMREAD_GRAYSCALE);
    if (src.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    Mat dst(src.size(), CV_32FC(6));
    cornerEigenValsAndVecs(src, dst, blockSize, ksize, borderType);

    // Extract the first eigenvalue
    vector<Mat> channels(6);
    split(dst, channels);
    Mat eigenvalue1 = channels[0];

    // Normalize for visualization
    normalize(eigenvalue1, eigenvalue1, 0, 255, NORM_MINMAX);
    eigenvalue1.convertTo(eigenvalue1, CV_8U);

    imwrite(outputPath, eigenvalue1);

    return enif_make_string(env, outputPath, ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 5, corner_eigenvals_and_vecs_nif}
};

ERL_NIF_INIT(corner_eigen, nif_funcs, NULL, NULL, NULL, NULL)
