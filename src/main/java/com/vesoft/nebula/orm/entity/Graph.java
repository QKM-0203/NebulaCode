/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.ngql.Encoding;
import com.vesoft.nebula.orm.util.Util;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Graph space object,it is obtained from {@link GraphService} according to
 * the name of the graph space.
 *
 * <p>You can add, delete, modify, update and obtain the data structure of graph space
 * through graph space objects.</p>
 *
 * @author Qi Kai Meng
 */
public class Graph {
    private final Session session;
    private final String spaceName;

    protected Graph(String spaceName, Session session) {
        this.spaceName = spaceName;
        this.session = session;
        ResultSet result = null;
        String useSpace = "USE " + "`" + spaceName + "`";
        result = run(useSpace);
        if (!result.isSucceeded()) {
            throw new ExecuteException(result.getErrorMessage());
        }
    }

    /**
     * execute sentence(ngql) statement.
     *
     * @param sentence sentence statement
     * @return execute result
     */
    public ResultSet run(String sentence)  {
        ResultSet resultSet = null;
        try {
            resultSet = session.execute(sentence);
        } catch (IOErrorException e) {
            e.printStackTrace();
        }
        if (resultSet == null) {
            throw new ExecuteException("session is broken");
        }
        return resultSet;
    }



    /**删除space下面所有
     * create graphObject.
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path.
     */
    public void create(Object graphObject) {
        if (graphObject == null) {
            throw new InitException("graphObject is null");
        }
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices,relationships,graphObject);
        createVertexes(vertices);
        createRelationships(relationships);
    }

    /**
     * delete graphObject.
     *
     * <p>delete vertex first delete relationship about vertex</p>
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
     */
    public void delete(Object graphObject) {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices,relationships,graphObject);
        deleteVertexes(vertices);
        deleteRelationships(relationships);
    }


    /**
     * delete some vertexes
     * @param vertices vertexList
     */
    public void deleteVertexes(List<Vertex> vertices) {
        ArrayList<String> vidList = new ArrayList<>();
        for (Vertex vertex : vertices) {
            vidList.add(vertex.getVid() instanceof String
                ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString());
        }
        ResultSet resultSet = run(String.format("DELETE VERTEX %s", String.join(",", vidList)));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * delete some relationships
     * @param relationships relationships
     */
    public void deleteRelationships(List<Relationship> relationships) {
        HashMap<String, List<Relationship>> classifyByEdge =
            Util.joinSameEdgeRelationships(relationships, 1);
        ArrayList<String>  relationshipString = new ArrayList<>();
        for (String edgeName : classifyByEdge.keySet()) {
            List<Relationship> relationshipList = classifyByEdge.get(edgeName);
            for (Relationship relationship : relationshipList) {
                relationshipString.add(
                    String.format("%s->%s@%d",relationship.getStartVid() instanceof String
                            ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                        relationship.getEndVid() instanceof String
                            ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                        relationship.getRank()));
            }
            ResultSet resultSet = run(String.format("DELETE EDGE `" + edgeName + "` %s",
                String.join(",", relationshipString)));
            if (!resultSet.isSucceeded()) {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        }
    }


    /**
     * insert some vertexes.
     *
     * <p>In this method, the points of the same tag form an insertion statement for operation,
     * Therefore, once an error occurs during the execution of a statement,
     * the user has to find out the errors of those points corresponding to the wrong tag.
     * Then ure execute the method. At the same time, when an error occurs,
     * the user will be returned which tags correspond to which vertexes are successfully executed,
     * which is convenient for the user to correct.</p>
     *
     * @param vertices vertexList
     */
    public void createVertexes(List<Vertex> vertices) {
        HashMap<String, List<Vertex>> classifyByTagAndProp = Util.joinSameTagVertices(vertices);
        ArrayList<String> vertexValues = new ArrayList<>();
        ArrayList<Vertex> success = new ArrayList<>();
        for (String tagJoin : classifyByTagAndProp.keySet()) {
            List<Vertex> vertexList = classifyByTagAndProp.get(tagJoin);
            for (Vertex vertex : vertexList) {
                vertexValues.add(Encoding.joinVertexValue(vertex));
            }
            ResultSet resultSet = run(String.format("INSERT VERTEX %s VALUES %s",
                tagJoin,String.join(",", vertexValues)));
            vertexValues.clear();
            if (!resultSet.isSucceeded()) {
                throw new ExecuteException(resultSet.getErrorMessage()
                + "\nSuccessful tag execution is " + success);
            }
            success.addAll(vertexList);
        }
    }

    /**
     * insert some relationships.
     *
     * <p>In this method, the points of the same edge form an insertion statement for operation,
     * Therefore, once an error occurs during the execution of a statement,
     * the user has to find out the errors of those relationships corresponding to the wrong edge.
     * Then re execute the method. At the same time, when an error occurs,
     * the user will be returned which edges correspond to which
     * relationships are successfully executed,which is convenient for the user to correct.</p>
     *
     * @param relationships relationships
     */
    public void createRelationships(List<Relationship> relationships) {
        HashMap<String, List<Relationship>> classifyByEdge =
            Util.joinSameEdgeRelationships(relationships,0);
        ArrayList<String> edgeValues = new ArrayList<>();
        ArrayList<Relationship> success = new ArrayList<>();
        for (String edgeJoin : classifyByEdge.keySet()) {
            List<Relationship> relationshipList = classifyByEdge.get(edgeJoin);
            for (Relationship relationship : relationshipList) {
                edgeValues.add(Encoding.joinRelationshipValue(relationship));
            }
            ResultSet resultSet = run(String.format("INSERT EDGE %s VALUES %s",
                edgeJoin,String.join(",", edgeValues)));
            edgeValues.clear();
            if (!resultSet.isSucceeded()) {
                throw new ExecuteException(resultSet.getErrorMessage()
                + "\nSuccessful edge execution is" + success);
            }
            success.addAll(relationshipList);
        }
    }


    /**
     * update graphObject.
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
     */
    public void update(Object graphObject) {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices,relationships,graphObject);
        updateVertexes(vertices);
        updateRelationship(relationships);
    }

    /**
     * update vertex tag attribute value
     * @param vertices vertices
     */
    public void updateVertexes(List<Vertex> vertices) {
        for (Vertex vertex : vertices) {
            HashMap<String, HashMap<String, Object>> propMap = vertex.getPropMap();
            for (String tagName : propMap.keySet()) {
                if (propMap.get(tagName) != null) {
                    String updateTagValue = Encoding.updateSchemaValue(propMap.get(tagName));
                    ResultSet resultSet = run(String.format("UPSERT VERTEX ON `%s` %s SET %s",
                        tagName, vertex.getVid() instanceof String
                            ? "\"" + vertex.getVid() + "\"" : vertex.getVid(), updateTagValue));
                    if (!resultSet.isSucceeded()) {
                        throw new ExecuteException(resultSet.getErrorMessage()
                            + "\n the tag " + tagName + " corresponding to " + vertex + " is fail");
                    }
                }
            }
        }
    }


    /**
     * update relationship edge attribute value
     * @param relationships relationships
     */
    public void updateRelationship(List<Relationship> relationships) {
        for (Relationship relationship : relationships) {
            HashMap<String, Object> propMap = relationship.getPropMap();
            if (propMap != null) {
                String updateEdgeValue = Encoding.updateSchemaValue(propMap);
                System.out.println(String.format("UPSERT EDGE ON `%s` %s->%s@%d SET %s",
                    relationship.getEdgeName(), relationship.getStartVid() instanceof String
                        ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                    relationship.getEndVid() instanceof String
                        ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                    relationship.getRank(), updateEdgeValue));
                ResultSet resultSet = run(String.format("UPSERT EDGE ON `%s` %s->%s@%d SET %s",
                    relationship.getEdgeName(), relationship.getStartVid() instanceof String
                        ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                    relationship.getEndVid() instanceof String
                        ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                    relationship.getRank(), updateEdgeValue));
                if (!resultSet.isSucceeded()) {
                    throw new ExecuteException(resultSet.getErrorMessage()
                        + "\n the edge " + relationship.getEdgeName()
                        + " corresponding to  " + relationship + " is fail");
                }
            }
        }
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
     * create an index with the specified name for tagName
     * according to the given attribute map, {@code Integer} is index length,
     * length is for string dataType,others can be directly transmitted to null
     *
     * @param tagName   tagName
     * @param indexName indexName
     * @param propList  indexList
     */
    public void createTagIndex(String tagName, String indexName,
                                     HashMap<String, Integer> propList) {
        ResultSet resultSet = run(String.format("CREATE TAG INDEX %s ON %s(%s)",
            indexName, tagName, Encoding.joinSIndexProp(propList)));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        ResultSet rebuildIndex = run(" REBUILD TAG INDEX " + indexName);
        if (!rebuildIndex.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * create an index with the specified name for  edgeName
     * according to the given attribute map, {@code Integer} is index length.
     *
     * @param edgeName  edgeName
     * @param indexName indexName
     * @param propList  indexList
     */
    public void createEdgeIndex(String edgeName, String indexName,
                                     HashMap<String, Integer> propList) {
        ResultSet resultSet = run(String.format("CREATE EDGE INDEX %s ON %s(%s)",
            indexName, edgeName, Encoding.joinSIndexProp(propList)));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        ResultSet rebuildIndex = run(" REBUILD EDGE INDEX " + indexName);
        if (!rebuildIndex.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }

    }

    /**
     * get all tagName from space.
     *
     * @return get tagNameList from space
     */
    public List<ValueWrapper> getTags()  {
        ResultSet resultSet = run("SHOW TAGS");
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet.colValues("Name");
    }

    /**
     * get all edgeName from space.
     *
     * @return get edgeNameList from space
     */
    public List<ValueWrapper> getEdges()  {
        ResultSet resultSet = run("SHOW EDGES");
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet.colValues("Name");
    }

    /**
     * delete Index by index name.
     *
     * @param indexName indexName
     */
    public void dropTagIndex(String indexName) {
        ResultSet resultSet = run("DROP TAG INDEX " + indexName);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * delete Index by index name.
     *
     * @param indexName indexName
     */
    public void dropEdgeIndex(String indexName) {
        ResultSet resultSet = run("DROP EDGE INDEX " + indexName);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
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
    public List<String> getEdgeIndexes(String edgeName) {
        return null;
    }


    /**
     * create tag.
     *
     * @param schema tagObject
     */
    public void createTag(Schema schema) {
        String tagJoin = "CREATE TAG " + Encoding.joinSchema(schema);
        ResultSet resultSet = run(tagJoin);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * create edge.
     *
     * @param schema edgeTypeObject
     */
    public void createEdge(Schema schema) {
        String edgeJoin = "CREATE EDGE " + Encoding.joinSchema(schema);
        ResultSet resultSet = run(edgeJoin);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

    /**
     * drop tag.
     *
     * @param tagName tagName
     */
    public void dropTag(String tagName) {
        ResultSet resultSet = null;
        resultSet = run("DROP TAG " + "`" + tagName + "`");
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * drop edge.
     *
     * @param edgeName edgeName
     */
    public void dropEdge(String edgeName) {
        ResultSet resultSet = null;
        resultSet = run("DROP EDGE " + "`" + edgeName + "`");
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

}
