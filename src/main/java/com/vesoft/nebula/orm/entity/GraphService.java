/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.client.graph.NebulaPoolConfig;
import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.AuthFailedException;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.exception.NotValidConnectionException;
import com.vesoft.nebula.client.graph.net.NebulaPool;
import com.vesoft.nebula.client.graph.net.Session;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.operator.DataType;
import com.vesoft.nebula.orm.query.cypher.Lexer;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

/**
 * {@code GraphService} Managing the entire Nebula graph cluster.
 *
 * <p>The user can pass in the host IP, port, user name and password,
 * establish a connection with the nebula service, and obtain the graph object
 * through the {@link #getGraph(String spaceName)} method.</p>
 *
 * @author Qi Kai Meng
 */
public class GraphService {
    private String username;
    private String password;
    private List<HostAddress> hostAddresses;
    private boolean reconnect = false;
    private NebulaPoolConfig nebulaPoolConfig = new NebulaPoolConfig();

    /**
     * pass in parameters use default connectionPool.
     */
    public GraphService(List<HostAddress> hostAddresses, String username, String password,
                        boolean reconnect) {
        this.hostAddresses = hostAddresses;
        this.username = username;
        this.password = password;
        this.reconnect = reconnect;

    }

    /**
     * pass in parameters use self-connectionPool config.
     */
    public GraphService(List<HostAddress> hostAddresses, String username, String password,
                        boolean reconnect, NebulaPoolConfig nebulaPoolConfig) {
        this.hostAddresses = hostAddresses;
        this.username = username;
        this.password = password;
        this.reconnect = reconnect;
        this.nebulaPoolConfig = nebulaPoolConfig;
    }

    public Session getSession() {
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
     * pass in spaceName get graph object.
     */
    public Graph getGraph(String spaceName) {
        return new Graph(spaceName, getSession());
    }

    public ResultSet spaces() {
        return null;
    }

    /**
     * create space by space objects.
     *
     * @param space spaceObject
     */
    public void createSpace(Space space) {
        if (space == null) {
            throw new NullPointerException("space object cannot be null");
        }
        if (space.getSpaceName() == null) {
            throw new IllegalArgumentException("spaceName cannot be null");
        }
        Session session = getSession();
        String createSpace;
        if (space.getVidDataType().equals(DataType.FIXED_STRING)) {
            createSpace = String.format(Lexer.CREATE + Lexer.SPACE + Lexer.IF_NOT_EXISTS + "`%s`"
                    + "(" + Lexer.PARTITION_NUM + "= %d," + Lexer.REPLICA_FACTOR + "= %d,"
                + Lexer.VID_TYPE + "= %s)",
                space.getSpaceName(), space.getPartitionNumber(),
                space.getReplicaFactor(), String.format("%s(%d" + ")",
                    space.getVidDataType(), space.getVidDataType().getLength()));
        } else {
            createSpace = String.format(Lexer.CREATE + Lexer.SPACE + Lexer.IF_NOT_EXISTS + "`%s`"
                    + "(" + Lexer.PARTITION_NUM + "= %d," + Lexer.REPLICA_FACTOR + "= %d,"
                    + Lexer.VID_TYPE + "= %s)",
                space.getSpaceName(), space.getPartitionNumber(),
                space.getReplicaFactor(), space.getVidDataType());
        }
        ResultSet result = run(createSpace);
        if (!result.isSucceeded()) {
            throw new ExecuteException(result.getErrorMessage());
        }
    }

    /**
     * execute sentence(nGgl) statement.
     *
     * @param sentence sentence statement
     * @return execute result
     */
    public ResultSet run(String sentence) {
        ResultSet resultSet = null;
        try {
            resultSet = getSession().execute(sentence);
        } catch (IOErrorException e) {
            e.printStackTrace();
        }
        if (resultSet == null) {
            throw new ExecuteException("session is broken");
        }
        return resultSet;
    }

    /**
     * delete space by spaceNameList.
     *
     * @param spaceNameList spaceNameList
     */
    public void dropSpaces(List<String> spaceNameList) {
        if (spaceNameList != null && !spaceNameList.isEmpty()) {
            ArrayList<String> spaceSentences = new ArrayList<>();
            for (String spaceName : spaceNameList) {
                spaceSentences.add(String.format(Lexer.DROP + Lexer.SPACE + Lexer.IF_EXISTS
                    + "`%s`", spaceName));
            }
            ResultSet resultSet = run(String.join(";", spaceSentences));
            if (!resultSet.isSucceeded()) {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        }
    }

    /**
     * get the user status of the cluster.
     *
     * @return users information
     */
    public ResultSet showUser() {
        return run(Lexer.SHOW + Lexer.USERS);
    }

    /**
     * get the host information of the cluster.
     *
     * @return cluster information
     */
    public ResultSet showHosts() {
        return run(Lexer.SHOW + Lexer.HOSTS);
    }

    /**
     * get the snapshot information of the cluster.
     *
     * @return configs information
     */
    public ResultSet getSnapshot() {
        return run(Lexer.SHOW + Lexer.SNAPSHOTS);
    }

    /**
     * get the part distribution in the graph space.
     *
     * @return space part information
     */
    public ResultSet getParts() {
        return run(Lexer.SHOW + Lexer.PARTS);
    }
}