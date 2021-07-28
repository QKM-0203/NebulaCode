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

import java.io.UnsupportedEncodingException;
import java.net.UnknownHostException;
import java.util.List;


public class GraphService  {

    private   String username;
    private   String password;
    private   List<HostAddress> hostAddresses;
    private   boolean reconnect = false;
    public    NebulaPoolConfig nebulaPoolConfig = new NebulaPoolConfig();



    /**
     * pass in parameters use default connectionPool
     */
    public GraphService(List<HostAddress> hostAddresses, String username, String password,boolean reconnect){
        this.hostAddresses = hostAddresses;
        this.username = username;
        this.password = password;
        this.reconnect = reconnect;

    }


    /**
     * pass in parameters use self-connectionPool config
     */
    public GraphService(List<HostAddress> hostAddresses,String username, String password,boolean reconnect,NebulaPoolConfig nebulaPoolConfig){
        this.hostAddresses = hostAddresses;
        this.username = username;
        this.password = password;
        this.reconnect = reconnect;
        this.nebulaPoolConfig = nebulaPoolConfig;
    }

    private Session getSession(){
        NebulaPool nebulaPool = new NebulaPool();
        Session session = null;
        try {
            nebulaPool.init(hostAddresses, nebulaPoolConfig);
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
            createSpace = String.format("CREATE SPACE IF NOT EXISTS %s(partition_num = %d,replica_factor = %d,vid_type = %s)"
                    , space.getSpaceName(), space.getPartitionNumber(), space.getReplicaFactor(),
                    String.format("%s(%d" + ")", space.getVidDateType(), space.getVidDateType().getLength()));
        } else {
            createSpace = String.format("CREATE SPACE IF NOT EXISTS %s(partition_num = %d,replica_factor = %d,vid_type = %s)"
                    , space.getSpaceName(), space.getPartitionNumber(), space.getReplicaFactor(),
                    space.getVidDateType());
        }

        ResultSet result = session.execute(createSpace);

        if(result == null){
            throw new ExecuteException("session is broken");
        }else return result.isSucceeded();

    }

    public boolean dropSpaces(List<String> spaceNameList) throws IOErrorException, UnsupportedEncodingException {
        if (spaceNameList == null) {
            return false;
        }
        Session session = getSession();
        for (String spaceName : spaceNameList) {
            session.execute(String.format("DROP IF EXISTS SPACE %s",spaceName));
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
