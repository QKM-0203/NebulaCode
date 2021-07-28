/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import Operator.DateType;
import com.vesoft.nebula.client.graph.NebulaPoolConfig;
import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.AuthFailedException;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.exception.NotValidConnectionException;
import com.vesoft.nebula.client.graph.net.NebulaPool;
import com.vesoft.nebula.client.graph.net.Session;
import error.ExecuteException;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;


public class GraphService  {

    public   String username = "root";
    public   String password = "nebula";
    public   String host = "127.0.0.1";
    public   int port = 9669;
    private boolean reconnect = false;
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
            if(pro.getProperty("reconnect") != null){
                reconnect = Boolean.parseBoolean(pro.getProperty("reconnect"));
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
    public GraphService(String host, int port, String username, String password,boolean reconnect){
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.reconnect = reconnect;

    }


    /**
     * pass in parameters use self-connectionPool config
     */
    public GraphService(String host, int port, String username, String password,boolean reconnect,NebulaPoolConfig nebulaPoolConfig){
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.reconnect = reconnect;
        this.nebulaPoolConfig = nebulaPoolConfig;
    }

    private Session getSession(){
        NebulaPool nebulaPool = new NebulaPool();
        Session session = null;
        List<HostAddress> addresses = Arrays.asList(new HostAddress(host,port));
        try {
            nebulaPool.init(addresses, nebulaPoolConfig);
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        try {
            session = nebulaPool.getSession(username, password, reconnect);
        } catch (NotValidConnectionException | IOErrorException | AuthFailedException e) {
            e.printStackTrace();
        }
        return session;
    }


    /**
     * pass in spaceName get Graph
     */
    public Graph getGraph(String spaceName) {
        return new Graph(spaceName,getSession());
    }

    public ResultSet spaces(){
        return null;
    }


    public boolean createSpace(Space space) throws IOErrorException, UnsupportedEncodingException {
        if (space == null) {
            throw new NullPointerException("space object cannot be null");
        }
        if(space.getSpaceName() == null) {
            throw new IllegalArgumentException("spaceName cannot be null");
        }
        Session session = getSession();
        String createSpace = null;
        if (space.getVidDateType().equals(DateType.FIXED_STRING)) {
            createSpace = String.format("CREATE SPACE %s(partition_num = %d,replica_factor = %d,vid_type = %s)"
                    , space.getSpaceName(), space.getPartitionNumber(), space.getReplicaFactor(),
                    String.format("%s(%d" + ")", space.getVidDateType(), space.getVidDateType().getLength()));
        } else {
            createSpace = String.format("CREATE SPACE %s(partition_num = %d,replica_factor = %d,vid_type = %s)"
                    , space.getSpaceName(), space.getPartitionNumber(), space.getReplicaFactor(),
                    space.getVidDateType());
        }

        ResultSet result = session.execute(createSpace);

        if(result == null){
            throw new ExecuteException("session is broken");
        }else return result.isSucceeded();

    }

    public boolean dropSpaces(List<String> spaceNameList){
        if (spaceNameList == null) {
            return false;
        }
        return true;
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
