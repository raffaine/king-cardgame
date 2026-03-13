#pragma once
#include <string>
#include <functional>

class HaskellBridge {
public:
    // Pass in the command line args to boot the RTS
    HaskellBridge(int argc, char* argv[]);
    ~HaskellBridge();

    // The method to start your ZMQ background threads
    void connectToServer(const std::string& subUrl, const std::string& pushUrl, 
                         const std::string& player, const std::string& password);

    // Provide a modern C++ callback for the C-API to trigger
    void setActionCallback(std::function<void(const std::string&)> callback);

    // Executes callback
    static void runCallback(const std::string&);

private:
    static std::function<void(const std::string&)> currentCallback;
};
