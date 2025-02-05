# WaoLlama

WaoLlama is a fully decentralized onchain LLM agent embedded in the browser on WAO.

```bash
cd waollama && yarn
```

It uses the wasm64 module from [aos-llama](https://github.com/samcamwilliams/aos-llama), so you need to enable `Experimental WebAssembly` from [chrome://flags](chrome://flags) to run it in the browser, if you are on Google Chrome.

Also, create `public/llama` directory, and download a gguf file from [Hugging Face](https://huggingface.co/TinyLlama/TinyLlama-1.1B-Chat-v1.0/tree/main), and put into the directory with the name `tynyllama.gguf`, and copy `src/lua/llama.wasm` into the directory. These files are not included due to their huge sizes.

Once everything is set, run the following.

```bash
yarn dev
```

Now your app is running at [localhost:3000](http://localhost:3000).
