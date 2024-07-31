#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to apply buildPyramid
static ERL_NIF_TERM build_pyramid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int maxlevel, borderType;
    if (!enif_get_int(env, argv[1], &maxlevel) || !enif_get_int(env, argv[2], &borderType)) {
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

    vector<Mat> dst;
    buildPyramid(src, dst, maxlevel, borderType);

    ERL_NIF_TERM result_list = enif_make_list(env, 0);
    for (int i = maxlevel; i >= 0; --i) {
        string outputPath = "pyramid_level_" + to_string(i) + ".jpg";
        imwrite(outputPath, dst[i]);
        ERL_NIF_TERM erl_outputPath = enif_make_string(env, outputPath.c_str(), ERL_NIF_LATIN1);
        result_list = enif_make_list_cell(env, erl_outputPath, result_list);
    }

    return result_list;
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 3, build_pyramid_nif}
};

ERL_NIF_INIT(build_pyramid, nif_funcs, nullptr, nullptr, nullptr, nullptr)
