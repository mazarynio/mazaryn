#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>
#include <vector>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Helper function to parse a list of rectangles from Erlang terms
vector<Rect> parse_rectangles(ErlNifEnv* env, ERL_NIF_TERM list) {
    unsigned int length;
    enif_get_list_length(env, list, &length);
    vector<Rect> rectangles(length);

    ERL_NIF_TERM head, tail = list;
    for (unsigned int i = 0; i < length; ++i) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) {
            throw runtime_error("Failed to get list cell");
        }

        int x, y, width, height;
        const ERL_NIF_TERM* tuple_elements;
        int arity;
        if (!enif_get_tuple(env, head, &arity, &tuple_elements) || arity != 4 ||
            !enif_get_int(env, tuple_elements[0], &x) ||
            !enif_get_int(env, tuple_elements[1], &y) ||
            !enif_get_int(env, tuple_elements[2], &width) ||
            !enif_get_int(env, tuple_elements[3], &height)) {
            throw runtime_error("Failed to parse rectangle");
        }
        rectangles[i] = Rect(x, y, width, height);
    }
    return rectangles;
}

// Function to perform Watershed segmentation
static ERL_NIF_TERM watershed_segment_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    cout << "Image path: " << imagePath << endl;
    if (!fs::exists(imagePath)) {
        cerr << "Error: File does not exist: " << imagePath << endl;
        return enif_make_string(env, "File does not exist", ERL_NIF_LATIN1);
    }

    Mat image = imread(imagePath, IMREAD_COLOR);
    if (image.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    vector<Rect> rectangles;
    try {
        rectangles = parse_rectangles(env, argv[1]);
    } catch (const exception& e) {
        cerr << e.what() << endl;
        return enif_make_badarg(env);
    }

    Mat markers = Mat::zeros(image.size(), CV_32S);

    for (int i = 0; i < rectangles.size(); ++i) {
        rectangle(markers, rectangles[i], Scalar(static_cast<int>(i) + 1), -1);
    }

    watershed(image, markers);

    Mat segmented(image.size(), CV_8UC3);
    for (int i = 0; i < markers.rows; ++i) {
        for (int j = 0; j < markers.cols; ++j) {
            int index = markers.at<int>(i, j);
            if (index == -1) {
                segmented.at<Vec3b>(i, j) = Vec3b(255, 255, 255);
            } else if (index <= 0 || index > rectangles.size()) {
                segmented.at<Vec3b>(i, j) = Vec3b(0, 0, 0);
            } else {
                segmented.at<Vec3b>(i, j) = image.at<Vec3b>(i, j);
            }
        }
    }

    string outputPath = "segmented_image_watershed.jpg";
    imwrite(outputPath, segmented);

    return enif_make_string(env, outputPath.c_str(), ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"watershed_segment", 2, watershed_segment_nif}
};

ERL_NIF_INIT(watershed, nif_funcs, NULL, NULL, NULL, NULL)
