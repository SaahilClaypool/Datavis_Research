cd ..
cd Prototype
npm run build
cp -Recurse build/* ../Server/wwwroot/ -Force
cd ..
cd Server