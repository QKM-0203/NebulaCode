/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import exception.ExecuteException;
import java.util.HashMap;
import java.util.List;

/**
 * Graph space object,it is obtained from {@link GraphService} according to
 * the name of the graph space.
 *
 * <p>You can add, delete, modify, update and obtain the data structure of graph space
 * through graph space objects.</p>
 */
public class Graph {
  private final Session session;

  protected Graph(String spaceName, Session session) {
    this.session = session;
    ResultSet  result = null;
    String useSpace = "USE " + spaceName;
    try {
      result = session.execute(useSpace);
    } catch (IOErrorException e) {
      e.printStackTrace();
    }
    if (result == null) {
      throw new ExecuteException("session is broken");
    }
    if (!result.isSucceeded()) {
      throw new ExecuteException("space is not found");
    }
  }

  /**
   * execute sentence(ngql) statement.
   *
   * @param sentence sentence statement
   * @return execute result
   * @throws IOErrorException IOErrorException when execute sentence
   */
  public ResultSet run(String sentence) throws IOErrorException {
    return session.execute(sentence);
  }

  /**
   * create graphObject.
   *
   * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path.
   */
  public void create(Object graphObject) {

  }

  /**
   * delete graphObject.
   *
   * <p>delete vertex first delete relationship about vertex</p>
   *
   * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
   */
  public void delete(Object graphObject) {

  }

  /**
   * delete all edges and vertex of space.
   */
  public void deleteAll() {

  }

  /**
   * update graphObject.
   *
   * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
   */
  public void update(Object graphObject) {

  }

  /**
   * determine whether the graphObject exists.
   *
   * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
   * @return whether exist
   */
  public boolean exists(Object graphObject) {
    return true;
  }

  /**
   * create an index with the specified name for tagName and or edgeName
   * according to the given attribute map, {@code Integer} is index length.
   *
   * @param name tagName or edgeName
   * @param indexName indexName
   * @param propList indexList
   * @return  whether create success
   */
  public boolean createSchemaIndex(String name, String indexName,
                                     HashMap<String, Integer> propList) {
    return true;
  }

  /**
   * get all tagName from space.
   *
   * @return get tagNameList from space
   */
  public List<String> getTags() {
    return null;
  }

  /**
   * get all edgeName from space.
   *
   * @return get edgeNameList from space
   */
  public List<String> getEdges() {
    return null;
  }

  /**
   * delete Index by index name.
   *
   * @param indexName indexName
   */
  public void dropIndex(String indexName) {

  }

  /**
   * get indexList by tagName.
   *
   * @param tagName tagName
   * @return indexList
   */
  public List<String> getTagIndexes(String tagName) {
    return null;
  }

  /**
   * get indexList by edgeName.
   *
   * @param edgeName edgeName
   * @return indexList
   */
  public List<String> getEdgeTypeIndexes(String edgeName) {
    return null;
  }

  /**
   * create schema.
   *
   * @param schema tagObject or edgeTypeObject
   * @return whether success
   */
  public boolean createSchema(Schema schema) {
    return true;
  }

  /**
   * drop schema.
   *
   * @param schemaName edgeName or tagName
   * @return whether drop success
   */
  public boolean dropSchema(String schemaName) {
    return true;
  }

  /**
   * add property or update property DateType or update TTL,
   * if is null don not modify.
   *
   * @param schemaName edgeName or tagName
   * @param schema tagObject or edgeTypeObject
   * @return whether modify success
   */
  public boolean modifySchema(String schemaName, Schema schema) {
    return true;
  }
}
