FROM ubuntu:14.10 

# docker build -t zole .
# docker run --name zole -p 9090:8080 -d zole
# docker stop zole
# docker start zole


EXPOSE 8080

ADD ./_build/prod/rel/zole_ws/zole_ws-0.1.2.tar.gz /opt/zole 

CMD ["/opt/zole/bin/zole_ws", "foreground"] 
