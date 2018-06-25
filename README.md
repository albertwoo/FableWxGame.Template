This project is trying to use Fsharp in wechat mini game development. It is just for fun. 

# Environmnet requirement
1. npm >= 5.6
2. dotnet core >= 2.0.0

# Dev
1. cd to ./src
2. run in watch mode: dotnet fable webpack -- -w
3. use "微信开发者工具" to open folder ./wx
4. change code and save

# Publish
1. dotnet fable webpack -- -p
2. use "微信开发者工具" to open folder ./wx and upload
