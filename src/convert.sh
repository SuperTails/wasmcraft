for f in ./intrinsic/i64divrem/*.mcfunction ; do sed -i -E 's/function wasm:(.*)/function intrinsic:i64divrem\/\1/' $f ; done
for f in ./intrinsic/i64divrem/*.mcfunction ; do sed -i -E 's/(%(lo|hi)%[0123])/\1%temp/g' $f ; done