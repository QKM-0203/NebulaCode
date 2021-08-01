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
import com.vesoft.nebula.orm.Common;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.ngql.Encoding;
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

    protected Graph(String spaceName, Session session) {
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
        Common.judgeGraphObject(vertices,relationships,graphObject);
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
        Common.judgeGraphObject(vertices,relationships,graphObject);
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
//        System.out.printf("DELETE VERTEX %s%n", String.join(",", vidList));
        ResultSet resultSet = run(String.format("DELETE VERTEX %s", String.join(",", vidList)));
        if (resultSet.isSucceeded()) {
            for (Vertex vertex : vertices) {
                vertex.setGraph(null);
            }
        } else {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * delete some relationships
     * @param relationships relationships
     */
    public void deleteRelationships(List<Relationship> relationships) {
        HashMap<String, List<Relationship>> classifyByEdge = Common.classifyByEdge(relationships, 1);
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
//            System.out.println(String.format("DELETE EDGE `" + edgeName + "` %s",
//                String.join(",", relationshipString)));
            ResultSet resultSet = run(String.format("DELETE EDGE `" + edgeName + "` %s",
                String.join(",", relationshipString)));
            if (resultSet.isSucceeded()) {
                for (Relationship relationship : relationshipList) {
                    relationship.setGraph(null);
                }
            } else {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        }
    }


    /**
     * insert some vertexes
     * @param vertices vertexList
     */
    public void createVertexes(List<Vertex> vertices) {
        HashMap<String, List<Vertex>> classifyByTagAndProp = Common.classifyByTagAndProp(vertices);
        ArrayList<String> vertexValues = new ArrayList<>();
        for (String tagJoin : classifyByTagAndProp.keySet()) {
            List<Vertex> vertexList = classifyByTagAndProp.get(tagJoin);
            for (Vertex vertex : vertexList) {
                vertexValues.add(Encoding.vertexValueJoin(vertex));
            }
            ResultSet resultSet = run(String.format("INSERT VERTEX %s VALUES %s",
                tagJoin,String.join(",", vertexValues)));
            vertexValues.clear();
            if (resultSet.isSucceeded()) {
                for (Vertex vertex : vertexList) {
                    vertex.setGraph(this);
                }
            } else {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        }
    }

    /**
     * insert some relationships
     * @param relationships relationships
     */
    public void createRelationships(List<Relationship> relationships) {
        HashMap<String, List<Relationship>> classifyByEdge = Common.classifyByEdge(relationships,0);
        ArrayList<String> edgeValues = new ArrayList<>();
        for (String edgeJoin : classifyByEdge.keySet()) {
            List<Relationship> relationshipList = classifyByEdge.get(edgeJoin);
            for (Relationship relationship : relationshipList) {
                edgeValues.add(Encoding.relationshipValueJoin(relationship));
            }
            ResultSet resultSet = run(String.format("INSERT EDGE %s VALUES %s",
                edgeJoin,String.join(",", edgeValues)));
            edgeValues.clear();
            if (resultSet.isSucceeded()) {
                for (Relationship relationship : relationshipList) {
                    relationship.setGraph(this);
                }
            } else {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        }
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
     * @param name      tagName or edgeName
     * @param indexName indexName
     * @param propList  indexList
     * @return whether create success
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
     */
    public void createSchema(Schema schema) {
        String tagJoin = Encoding.schemaJoin(schema);
        ResultSet resultSet = run(tagJoin);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        schema.setGraph(this);
    }

    /**
     * drop schema.
     *
     * @param schemaName edgeName or tagName
     * @param flag edge or tag,flag == 0
     */
    public void dropSchema(String schemaName,int flag) {
        ResultSet resultSet = null;
        if (flag == 0) {
            resultSet = run("DROP TAG " + "`" + schemaName + "`");
        } else {
            resultSet = run("DROP EDGE " + "`" + schemaName + "`");
        }
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

    /**
     * add property or update property DateType or update TTL,
     * if is null don not modify.
     *
     * @param schema  tagObject or edgeTypeObject
     * @return whether modify success
     */
    public boolean modifySchema(Schema schema) {
        return true;
    }



}
