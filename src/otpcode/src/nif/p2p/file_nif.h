#ifndef FILE_NIF_H
#define FILE_NIF_H

#include <string>
#include <filesystem>
#include <erl_nif.h>

// File class declaration
class File {
private:
    std::string name;
    std::size_t file_size;
    std::string hash;

public:
    File(const std::string& name_);
    std::string getName() const;
    std::size_t getFileSize() const;
    void setFileSize(std::size_t size);
    std::string getHash() const;
};

#endif // FILE_NIF_H
