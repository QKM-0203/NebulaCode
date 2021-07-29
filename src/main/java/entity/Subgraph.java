/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

import error.InitException;
import java.util.List;

/**
 * a subgraph is a set of points and edges.
 */
public class Subgraph {
  private List<Vertex> vertexList;
  private List<Relationship> relationshipsList;

  public Subgraph(List<Vertex> vertexList, List<Relationship> relationshipsList) {
    init(vertexList, relationshipsList);
  }

  protected void init(List<Vertex> vertexList, List<Relationship> relationshipsList) {
    this.vertexList = vertexList;
    this.relationshipsList = relationshipsList;
    if (vertexList == null || vertexList.isEmpty()) {
      throw new InitException("the subgraph contains at least one point");
    }
  }

  /**
   * get all vertexes from subgraph.
   *
   * @return collection of all vertexes
   */
  public  List<Vertex>  vertexes() {
    if (vertexList == null || vertexList.isEmpty()) {
      return null;
    }
    return  vertexList;
  }

  /**
   * get all relationship from subgraph.
   *
   * @return collection of all relationships
   */
  public List<Relationship> relationships() {
    if (relationshipsList == null || relationshipsList.isEmpty()) {
      return null;
    }
    return relationshipsList;
  }

  /**
   * gets the graph space object bound by the subgraph.
   *
   * @return graph space object
   */
  public Graph graph() {
    if ((vertexList == null || vertexList.isEmpty())
        && (relationshipsList == null || relationshipsList.isEmpty())) {
      return null;
    } else {
      if (vertexList != null && !vertexList.isEmpty()) {
        return vertexList.get(0).getGraph();
      } else {
        return relationshipsList.get(0).getGraph();
      }
    }
  }

  //{"Person", "Employee"}
  public List<String> tags() {
    return null;
  }

  //{"KNOWS", "LIKES", "DISLIKES","MARRIED_TO", "WORKS_FOR"}
  public List<String> types() {
    return null;
  }

  public List<Property> properties() {
    return null;
  }

  @Override
  public String toString() {
    return "Subgraph{"
        + "vertex" + vertexList
        + ", relationships" + relationshipsList
        + '}';
  }
}
