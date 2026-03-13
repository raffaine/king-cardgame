#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 0) out vec2 fragTexCoord;

// NEW: Receive the aspect ratio from C++
layout(push_constant) uniform PushConstants {
    float aspectRatio;
} push;

void main() {
    // Squeeze the X coordinate based on the screen's width/height ratio
    gl_Position = vec4(inPosition.x * push.aspectRatio, inPosition.y, 0.0, 1.0);
    fragTexCoord = inTexCoord;
}
