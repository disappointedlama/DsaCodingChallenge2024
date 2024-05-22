# Visualizer 
## Run Bash
```bash
    docker build -t visualizer . && docker run -it --rm  -v $(pwd)/../..:/app/result_files  -v $(pwd)/../../../input_files:/app/input_files -p 8081:8081 -p 3123:3123 visualizer
```

## Run Powershell
```powershell
    docker build -t visualizer . ; docker run -it --rm  -v ${pwd}/../..:/app/result_files  -v ${pwd}/../../../input_files:/app/input_files -p 8081:8081 -p 3123:3123 visualizer
```