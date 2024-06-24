#ifndef HASH_H
#define HASH_H

#include <string>

class Hash {
public:
    static std::string computeSHA256(const std::string& filePath);
};

#endif // HASH_H
