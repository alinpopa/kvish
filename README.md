### Configuration:

  + by default the `kvish` system uses the following ports:  
    - rest interface port `9991` (can be modified within `kvish.app.src`)  
    - tcp interface port `9992` (can be modified within `kvish.app.src`)  

### Run:

  - `make start`  

### Protocol:

  + in order to support arbitrary data types, the format of the value will be binary, therefore all examples from here will have the erlang binary format  
  + request format  
    - a request will be comprise of only one line, each line having 3 parts separated by a space (e.g. `<<"COMMAND KEY VALUE">>`);  
    - each command/request will be ended by a carriage return;  
    - there are only 2 types of the requests:  
      - GET - `<<"GET KEY">>`  
      - PUT - `<<"PUT KEY value">>`  
    - for each request, there will be a reply, binary as well;  
    - the following replies are available:  
      - request: `<<"GET some_key">>`, response: `<<"VALUE:value_content">>`;  
      - request: `<<"PUT some_key value_to_be_stored">>`, response: `<<"OK">>`;  
      - if case of any error (including key not found), the response will be `<<"ERROR:reason">>`;  

