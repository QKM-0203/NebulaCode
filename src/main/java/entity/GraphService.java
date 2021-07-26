/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import com.vesoft.nebula.client.graph.NebulaPoolConfig;
import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.AuthFailedException;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.exception.NotValidConnectionException;
import com.vesoft.nebula.client.graph.net.NebulaPool;
import com.vesoft.nebula.client.graph.net.Session;

import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;


public class GraphService  {

    public   String username = "root";
    public   String password = "nebula";
    public   String host = "127.0.0.1";
    public   int port = 9669;
    public   NebulaPoolConfig nebulaPoolConfig = new NebulaPoolConfig();

    /**
     * read configuration file (nebula.properties)
     */
    public GraphService() {
            Properties pro = new Properties();
            //use classLoader to load the configuration file and get the byte input stream
            InputStream is = GraphService.class.getClassLoader().getResourceAsStream("nebula.properties");
            try {
               pro.load(is);
            } catch (IOException e) {
               e.printStackTrace();
            }
            if(pro.getProperty("username") != null){
                username = pro.getProperty("username");
            }
            if(pro.getProperty("password") != null){
                password = pro.getProperty("password");
            }
            if(pro.getProperty("maxConnSize") != null){
                nebulaPoolConfig.setMaxConnSize(Integer.parseInt(pro.getProperty("maxConnSize")));
            }
            if(pro.getProperty("minConnSize") != null){
                nebulaPoolConfig.setMinConnSize(Integer.parseInt(pro.getProperty("minConnSize")));
            }
            if(pro.getProperty("timeout") != null){
                nebulaPoolConfig.setTimeout(Integer.parseInt(pro.getProperty("timeout")));
            }
            if(pro.getProperty("idleTime") != null){
                nebulaPoolConfig.setIdleTime(Integer.parseInt(pro.getProperty("idleTime")));
            }
            if(pro.getProperty("port") != null){
                port = Integer.parseInt(pro.getProperty("port"));
            }
            if(pro.getProperty("host") != null){
                host = pro.getProperty("host");
            }
    }


    /**
     * pass in parameters use default connectionPool
     */
    public GraphService(String host, int port, String username, String password){
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;

    }


    /**
     * pass in parameters use self-connectionPool config
     */
    public GraphService(String host, int port, String username, String password, NebulaPoolConfig nebulaPoolConfig){
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.nebulaPoolConfig = nebulaPoolConfig;
    }

    private Session getSession(boolean reconnect){
        NebulaPool nebulaPool = new NebulaPool();
        Session session = null;
        List<HostAddress> addresses = Arrays.asList(new HostAddress(host,port));
        try {
            nebulaPool.init(addresses, nebulaPoolConfig);
            session = nebulaPool.getSession(username, password, reconnect);
        } catch (UnknownHostException | NotValidConnectionException | IOErrorException | AuthFailedException e) {
            e.printStackTrace();
        }
        return session;
    }


    /**
     * pass in spaceName get Graph
     */
    public Graph getGraph(String spaceName,boolean reconnect){
        return new Graph(spaceName,getSession(reconnect));
    }

    public ResultSet spaces(){
        return null;
    }

    public void createSpace(Space space){

    }

    public void dropSpaces(List<String> spaceNameList){

    }


    public ResultSet showUser(){
        return null;
    }

    public ResultSet  showHosts(){
        return null;
    }

    public ResultSet getConfigs(){
        return null;
    }

    public ResultSet getParts(){
        return null;
    }



}
