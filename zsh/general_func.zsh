rmcdir() {
    rm -rf -- "$(pwd -P)" && cd ..
}

ag3() {
    ask_gpt --model "gpt-3.5-turbo"
}

ag4() {
    ask_gpt --model "gpt-4-turbo-preview"
}
