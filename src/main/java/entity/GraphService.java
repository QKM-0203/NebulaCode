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
import error.ExecuteException;
import java.net.UnknownHostException;
import java.util.List;
import operator.DateType;


/**
 * {@code GraphService} Managing the entire Nebula graph cluster.
 *
 *<p>The user can pass in the host IP, port, user name and password,
 * establish a connection with the nebula service, and obtain the graph object
 * through the {@link #getGraph(String spaceName)} method.</p>
 */
public class GraphService  {
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

  private Session getSession() {
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
   * @return whether create space success
   * @throws IOErrorException IOErrorException when execute createSpace sentence
   */
  public boolean createSpace(Space space) throws IOErrorException {
    if (space == null) {
      throw new NullPointerException("space object cannot be null");
    }
    if (space.getSpaceName() == null) {
      throw new IllegalArgumentException("spaceName cannot be null");
    }
    Session session = getSession();
    String createSpace = null;
    if (space.getVidDateType().equals(DateType.FIXED_STRING)) {
      createSpace = String.format("CREATE SPACE IF NOT EXISTS %s"
              + "(partition_num = %d,replica_factor = %d,vid_type = %s)",
          space.getSpaceName(), space.getPartitionNumber(), space.getReplicaFactor(),
          String.format("%s(%d" + ")", space.getVidDateType(), space.getVidDateType().getLength()));
    } else {
      createSpace = String.format("CREATE SPACE IF NOT EXISTS %s"
              + "(partition_num = %d,replica_factor = %d,vid_type = %s)",
          space.getSpaceName(), space.getPartitionNumber(), space.getReplicaFactor(),
          space.getVidDateType());
    }

    ResultSet result = session.execute(createSpace);
    if (result == null) {
      throw new ExecuteException("session is broken");
    } else {
      return result.isSucceeded();
    }

  }

  /**
   * delete space by spaceNameList.
   *
   * @param spaceNameList spaceNameList
   * @return whether drop space success
   * @throws IOErrorException IOErrorException when execute dropSpace sentence
   */
  public boolean dropSpaces(List<String> spaceNameList) throws IOErrorException {
    if (spaceNameList == null) {
      return false;
    }
    Session session = getSession();
    for (String spaceName : spaceNameList) {
      session.execute(String.format("DROP IF EXISTS SPACE %s", spaceName));
    }
    return true;
  }

  /**
   * get the user status of the cluster.
   *
   * @return users information
   */
  public ResultSet showUser() {
    return null;
  }

  /**
   * get the host information of the cluster.
   *
   * @return cluster information
   */
  public ResultSet  showHosts() {
    return null;
  }

  /**
   * get the configuration information of the cluster.
   *
   * @return configs information
   */
  public ResultSet getConfigs() {
    return null;
  }

  /**
   * get the part distribution in the graph space.
   *
   * @return space part information
   */
  public ResultSet getParts() {
    return null;
  }
}
