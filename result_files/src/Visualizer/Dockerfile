FROM node:18.15.0-slim

WORKDIR /app

COPY * /app/

RUN mkdir /app/input_files
RUN mkdir /app/result_files

RUN npm install

CMD ["node", "server.js"]
