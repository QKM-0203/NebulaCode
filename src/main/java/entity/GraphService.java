package entity;


import com.vesoft.nebula.client.graph.NebulaPoolConfig;


public class GraphService extends Graph {

    public GraphService(String host, int port, String username,String password,String spaceName,NebulaPoolConfig nebulaPoolConfig){
        super(host, port, username, password, spaceName,nebulaPoolConfig);
    }

    public GraphService(String host, int port, String username,String password,String spaceName){
       super(host, port, username, password, spaceName);
    }





}
