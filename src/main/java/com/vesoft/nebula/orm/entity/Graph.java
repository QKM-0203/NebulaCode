/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.client.graph.data.Node;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.util.KeyWord;
import com.vesoft.nebula.orm.query.util.Util;
import java.io.UnsupportedEncodingException;
import java.util.*;

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
    private static final String VERTICES_ = "vertices_";
    private static final String EDGES_ = "edges_";
    private static final String NAME = "Name";
    private static final String COUNT = "Count";
    private static final String VERTICES = "vertices";
    private static final String EDGES = "edges";
    private static final String FIELD = "Field";

    protected Graph(String spaceName, Session session) {
        this.session = session;
        ResultSet result;
        String useSpace = KeyWord.USE + " `" + spaceName + "`";
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
    public ResultSet run(String sentence) {
        try {
            return session.execute(sentence);
        } catch (IOErrorException e) {
            throw new ExecuteException(e.getMessage());
        }
    }

    /**
     * create graphObject.
     *
     * <p> For points, points with the same tag are placed in a collection
     * for batch insertion.In the operation of batch insertion,
     * there will be a problem that a point cannot be successfully executed.
     * Therefore, once the creation fails, the user will find out the error and correct it,
     * and then execute the statement again.</p>
     *
     * <p> For relationship, relationships with the same edge are placed in a collection
     * for batch insertion.In the operation of batch insertion,
     * there will be a problem that a point cannot be successfully executed.
     * Therefore, once the creation fails, the user will find out the error and correct it,
     * and then execute the statement again.</p>
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path.
     */
    public void create(Object graphObject) {
        if (graphObject == null) {
            throw new InitException("graphObject is null");
        }
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices, relationships, graphObject);
        Map<String, List<Vertex>> joinSameTagVertices = Util.joinSameTagVertices(vertices);
        if (vertices.size() != 0) {
            ArrayList<String> vertexValues = new ArrayList<>();
            ArrayList<Vertex> successVertexes = new ArrayList<>();
            for (String tagJoin : joinSameTagVertices.keySet()) {
                List<Vertex> vertexList = joinSameTagVertices.get(tagJoin);
                for (Vertex vertex : vertexList) {
                    vertexValues.add(Encoding.joinVertexValue(vertex));
                }
                ResultSet resultSet = run(String.format(KeyWord.INSERT + " " + KeyWord.VERTEX
                        + " %s " + KeyWord.VALUES + " %s",
                    tagJoin, String.join(",", vertexValues)));
                vertexValues.clear();
                if (!resultSet.isSucceeded()) {
                    throw new ExecuteException(resultSet.getErrorMessage()
                        + "\nSuccessful tag execution is " + successVertexes);
                }
                for (Vertex vertex : vertexList) {
                    vertex.setGraph(this);
                }
                successVertexes.addAll(vertexList);
            }
        }
        if (relationships.size() != 0) {
            HashMap<String, List<Relationship>> joinSameEdgeRelationships =
                Util.joinSameEdgeRelationships(relationships, 0);
            ArrayList<String> edgeValues = new ArrayList<>();
            ArrayList<Relationship> successEdges = new ArrayList<>();
            for (String edgeJoin : joinSameEdgeRelationships.keySet()) {
                List<Relationship> relationshipList = joinSameEdgeRelationships.get(edgeJoin);
                for (Relationship relationship : relationshipList) {
                    edgeValues.add(Encoding.joinRelationshipValue(relationship));
                }
                ResultSet resultSet = run(String.format(KeyWord.INSERT + " " + KeyWord.EDGE
                        + " %s " + KeyWord.VALUES + " %s",
                    edgeJoin, String.join(",", edgeValues)));
                edgeValues.clear();
                if (!resultSet.isSucceeded()) {
                    throw new ExecuteException(resultSet.getErrorMessage()
                        + "\nSuccessful edge execution is " + successEdges);
                }
                for (Relationship relationship : relationshipList) {
                    relationship.setGraph(this);
                }
                successEdges.addAll(relationshipList);
            }
        }
    }

    /**
     * delete graphObject.
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
     */
    public void delete(Object graphObject) {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices, relationships, graphObject);
        if (vertices.size() != 0) {
            ArrayList<Object> delSuccess = new ArrayList<>();
            for (Vertex vertex : vertices) {
                ResultSet resultSet = run(String.format(KeyWord.DELETE + " "
                        + KeyWord.VERTEX + " %s",
                    vertex.getVid() instanceof String
                        ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString()));
                if (!resultSet.isSucceeded()) {
                    throw new ExecuteException(resultSet.getErrorMessage()
                        + "\nthe id are " + delSuccess + ",the deletion is successful");
                }
                delSuccess.add(vertex.getVid());
            }
        }
        if (relationships.size() != 0) {
            HashMap<String, List<Relationship>> joinSameEdgeRelationships =
                Util.joinSameEdgeRelationships(relationships, 1);
            ArrayList<String> relationshipString = new ArrayList<>();
            ArrayList<String> delSuccess = new ArrayList<>();
            for (String edgeName : joinSameEdgeRelationships.keySet()) {
                List<Relationship> relationshipList = joinSameEdgeRelationships.get(edgeName);
                for (Relationship relationship : relationshipList) {
                    relationshipString.add(
                        String.format("%s->%s@%d",
                            relationship.getStartVid() instanceof String
                                ? "\"" + relationship.getStartVid() + "\"" :
                                relationship.getStartVid(),
                            relationship.getEndVid() instanceof String
                                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                            relationship.getRank()));
                }
                ResultSet resultSet = run(String.format(KeyWord.DELETE + " " + KeyWord.EDGE
                        + " `" + edgeName + "` %s",
                    String.join(",", relationshipString)));
                if (!resultSet.isSucceeded()) {
                    throw new ExecuteException(resultSet.getErrorMessage()
                        + "\nthe edge are " + delSuccess + ",the deletion is successful");
                }
                delSuccess.add(edgeName);
            }
        }
    }


    /**
     * The merge method only judges according to the passed in parameters
     * schemaName and propName,so it is only part of the condition matching,
     * if schema has no attribute,you can pass in null or not pass.
     *
     * <p>For a point, If the corresponding point is found in the remote according
     * to the tagName and value of propName, the passed point object
     * will be overwritten to the remote,otherwise insert the point.</p>
     * <p>For edges, existence is to update the attribute value, otherwise it is to insert.</p>
     * <p>if you merge relationship,you can not pass in schemaName and propName,because
     * relationship object has edgeName,if not exist this method can create,
     * otherwise update attribute value</p>
     *
     * <p>If you pass in a subgraph or path, the schemaName
     * you pass in is applied to all points in the subgraph or path</p>
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path.
     */
    public void merge(Object graphObject, String... schema) throws UnsupportedEncodingException {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices, relationships, graphObject);
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                create(relationship);
            }
        }
        if (vertices.size() != 0) {
            if (schema[0] == null) {
                throw new ExecuteException("tagName is "
                    + "a condition of merge,so cannot be null");
            }
            ResultSet resultSet = run(KeyWord.DESC + " " + KeyWord.TAG
                + " " + schema[0]);
            if (resultSet.isSucceeded() && !resultSet.isEmpty()) {
                List<ValueWrapper> field = resultSet.colValues(FIELD);
                ArrayList<String> filedString = new ArrayList<>();
                for (ValueWrapper filed : field) {
                    filedString.add(filed.asString());
                }
                if (filedString.contains(schema[1])) {
                    for (Vertex vertex : vertices) {
                        create(vertex);
                    }
                } else {
                    throw new ExecuteException("tag " + schema[0]
                        + " without " + "attribute " + schema[1]);
                }
            } else if (resultSet.isSucceeded() && resultSet.isEmpty()) {
                for (Vertex vertex : vertices) {
                    create(vertex);
                }
            } else {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        }
    }


    /**
     * pull graph objects from a remote location.
     *
     * <p>For a point, pull all the information of the point according to its ID.</p>
     * <p>For an edge, all information of the edge is obtained according to the edge type,
     * start ID, end ID, and rank of the edge.</p>
     * <p>If the remote entity does not exist, the data transmitted
     * by the original user is maintained.</p>
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path.
     * @throws UnsupportedEncodingException valueWrapper conversion exception
     */
    public void pull(Object graphObject) throws UnsupportedEncodingException {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices, relationships, graphObject);
        if (vertices.size() != 0) {
            HashMap<String, Vertex> idVertexMap = new HashMap<>();
            for (Vertex vertex : vertices) {
                idVertexMap.put(vertex.getVid().toString(), vertex);
            }
            ResultSet vertexResultSet = judgeExistVertexes(vertices);
            List<ValueWrapper> remoteVertices = vertexResultSet.colValues(VERTICES_);
            for (ValueWrapper remoteVertex : remoteVertices) {
                Node node = remoteVertex.asNode();
                Vertex vertex;
                if (node.getId().isString()) {
                    vertex = idVertexMap.get(node.getId().asString());
                } else {
                    vertex = idVertexMap.get(node.getId().toString());
                }
                HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
                for (String tagName : node.tagNames()) {
                    HashMap<String, ValueWrapper> properties = node.properties(tagName);
                    HashMap<String, Object> tagMap = new HashMap<>();
                    Util.judgeValueWrapper(tagMap, properties);
                    propMap.put(tagName, tagMap);
                }
                vertex.setGraph(this);
                vertex.setPropMap(propMap);
            }
        }
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                ResultSet resultSet = judgeExistEdge(relationship);
                List<ValueWrapper> remoteEdges = resultSet.colValues(EDGES_);
                HashMap<String, ValueWrapper> properties =
                    remoteEdges.get(0).asRelationship().properties();
                HashMap<String, Object> edgeMap = new HashMap<>();
                Util.judgeValueWrapper(edgeMap, properties);
                relationship.setPropMap(edgeMap);
                relationship.setGraph(this);
            }
        }
    }

    /**
     * push local data to remote database,full coverage.
     *
     * <p>remoteTags is first from remote pull,
     * so in most scenarios, what can be guaranteed is the latest</p>
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path
     */
    public void push(Object graphObject) throws UnsupportedEncodingException {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices, relationships, graphObject);
        if (vertices.size() != 0) {
            HashMap<String, Vertex> idVertexMap = new HashMap<>();
            for (Vertex vertex : vertices) {
                idVertexMap.put(vertex.getVid().toString(), vertex);
            }
            ResultSet resultSet = judgeExistVertexes(vertices);
            List<ValueWrapper> remoteVertex = resultSet.colValues(VERTICES_);
            for (ValueWrapper value : remoteVertex) {
                Node node = value.asNode();
                Vertex vertex;
                if (node.getId().isString()) {
                    vertex = idVertexMap.get(node.getId().asString());
                } else {
                    vertex = idVertexMap.get(node.getId().toString());
                }
                List<String> remoteTags = node.labels();
                Set<String> localTags = vertex.getPropMap().keySet();
                //update and add tag
                create(vertex);
                //delete tag,TODO: depends on  https://github.com/vesoft-inc/nebula-graph/pull/1303
                if (remoteTags.removeAll(localTags)) {
                    System.out.println(remoteTags);
                }
            }
        }
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                ResultSet resultSet = judgeExistEdge(relationship);
                List<ValueWrapper> remoteEdge = resultSet.colValues(EDGES_);
                if (remoteEdge.size() != 0) {
                    create(relationship);
                }
            }
        }
    }


    /**
     * TODO
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
                               Map<String, Integer> propList) {
        ResultSet resultSet = run(String.format(KeyWord.CREATE + " " + KeyWord.TAG
                + " " + KeyWord.INDEX + " "
                + KeyWord.IF_NOT_EXISTS + " %s " + KeyWord.ON + " %s(%s)",
            indexName, tagName, (Encoding.joinIndexProp(propList)) == null
                ? "" : Encoding.joinIndexProp(propList)));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * create an index with the specified name for  edgeName
     * according to the given attribute map, {@code Integer} is index length,
     * only dataType is String has index length,so other pass in null.
     *
     * @param edgeName  edgeName
     * @param indexName indexName
     * @param propList  indexList
     */
    public void createEdgeIndex(String edgeName, String indexName,
                                Map<String, Integer> propList) {
        ResultSet resultSet = run(String.format(KeyWord.CREATE + " " + KeyWord.EDGE + " "
                + KeyWord.INDEX + " " + KeyWord.IF_NOT_EXISTS + " %s " + KeyWord.ON + " %s(%s)",
            indexName, edgeName, (Encoding.joinIndexProp(propList)) == null
                ? "" : Encoding.joinIndexProp(propList)));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

    /**
     * get all tagName from space.
     *
     * @return get tagNameList from space
     */
    public List<String> getTags() {
        ArrayList<String> tagNames = new ArrayList<>();
        run(KeyWord.SHOW + " " + KeyWord.TAGS).colValues(NAME)
            .forEach(tagName -> {
                try {
                    tagNames.add(tagName.asString());
                } catch (UnsupportedEncodingException e) {
                    e.printStackTrace();
                }
            });
        return tagNames;
    }


    /**
     * get all edgeName from space.
     *
     * @return get edgeNameList from space
     */
    public List<String> getEdges() {
        ArrayList<String> edgeNames = new ArrayList<>();
        run(KeyWord.SHOW + " " + KeyWord.EDGES).colValues(NAME)
            .forEach(tagName -> {
                try {
                    edgeNames.add(tagName.asString());
                } catch (UnsupportedEncodingException e) {
                    e.printStackTrace();
                }
            });
        return edgeNames;
    }

    /**
     * delete Index by index name.
     *
     * @param indexName indexName
     */
    public void dropTagIndex(String indexName) {
        StringBuffer tagIndexJoin = new StringBuffer();
        tagIndexJoin.append(KeyWord.DROP).append(" ").append(KeyWord.TAG).append(" ")
            .append(KeyWord.INDEX).append(" ").append(KeyWord.IF_EXISTS).append(" ")
            .append(indexName);
        ResultSet resultSet = run(tagIndexJoin.toString());
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
        StringBuffer edgeIndexJoin = new StringBuffer();
        edgeIndexJoin.append(KeyWord.DROP).append(" ").append(KeyWord.EDGE).append(" ")
            .append(KeyWord.INDEX).append(" ").append(KeyWord.IF_EXISTS).append(" ")
            .append(indexName);
        ResultSet resultSet = run(edgeIndexJoin.toString());
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

    /**
     * TODO
     * get indexList by tagName.
     *
     * @param tagName tagName
     * @return indexList
     */
    public List<String> getTagIndexes(String tagName) {
        return null;
    }

    /**
     * TODO
     * get indexList by edgeName.
     *
     * @param edgeName edgeName
     * @return indexList
     */
    public List<String> getEdgeIndexes(String edgeName) {
        return null;
    }


    /**
     * create a tag.
     *
     * @param schema tagObject
     */
    public void createTag(Schema schema) {
        StringBuffer tagJoin = new StringBuffer();
        tagJoin.append(KeyWord.CREATE).append(" ").append(KeyWord.TAG).append(" ")
            .append(KeyWord.IF_NOT_EXISTS).append(" ").append(Encoding.joinSchema(schema));
        ResultSet resultSet = run(tagJoin.toString());
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }


    /**
     * create a edge.
     *
     * @param schema edgeTypeObject
     */
    public void createEdge(Schema schema) {
        StringBuffer edgeJoin = new StringBuffer();
        edgeJoin.append(KeyWord.CREATE).append(" ").append(KeyWord.EDGE).append(" ")
            .append(KeyWord.IF_NOT_EXISTS).append(" ").append(Encoding.joinSchema(schema));
        ResultSet resultSet = run(edgeJoin.toString());
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

    /**
     * drop some tag.
     *
     * @param tagNameList tagName
     */
    public void dropTagList(List<String> tagNameList) {
        ArrayList<String> tagSentence = new ArrayList<>();
        for (String tagName : tagNameList) {
            StringBuffer tagJoin = new StringBuffer();
            tagJoin.append(KeyWord.DROP).append(" ").append(KeyWord.TAG).append(" ")
                .append(KeyWord.IF_EXISTS).append(" `").append(tagName).append("`");
            tagSentence.add(tagJoin.toString());
        }
        ResultSet resultSet = run(String.join(";", tagSentence));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }

    }


    /**
     * drop some edge.
     *
     * @param edgeNameList edgeName
     */
    public void dropEdgeList(List<String> edgeNameList) {
        ArrayList<String> edgeSentence = new ArrayList<>();
        for (String edgeName : edgeNameList) {
            StringBuffer edgeJoin = new StringBuffer();
            edgeJoin.append(KeyWord.DROP).append(" ").append(KeyWord.EDGE).append(" ")
                .append(KeyWord.IF_EXISTS).append(" `").append(edgeName).append("`");
            edgeSentence.add(edgeJoin.toString());
        }
        ResultSet resultSet = run(String.join(";", edgeSentence));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }

    }

    /**
     * drop a tag
     *
     * @param tagName tagName
     */
    public void dropTag(String tagName) {
        ArrayList<String> tagList = new ArrayList<>();
        tagList.add(tagName);
        dropTagList(tagList);
    }

    /**
     * drop a edge
     *
     * @param edgeName edgeName
     */
    public void dropEdge(String edgeName) {
        ArrayList<String> edgeList = new ArrayList<>();
        edgeList.add(edgeName);
        dropEdgeList(edgeList);
    }

    /**
     * gets the number of vertexes of the specified tag by tagName,
     * you should execute submit job stats before executing this method,
     * if you pass in null means get all vertexes number
     *
     * @param tagName Specified tag name
     * @return vertexNumber
     * @throws UnsupportedEncodingException revert type is fail
     */
    public long vertexNumber(String tagName) throws UnsupportedEncodingException {
        ResultSet showStats = run(KeyWord.SHOW + " " + KeyWord.STATS);
        if (!showStats.isSucceeded()) {
            throw new ExecuteException(showStats.getErrorMessage());
        }
        if (showStats.rowsSize() != 0) {
            if (tagName == null) {
                tagName = VERTICES;
            }
            for (int i = 0; i < showStats.rowsSize(); i++) {
                ResultSet.Record valueWrappers = showStats.rowValues(i);
                if (valueWrappers.get(NAME).asString().equals(tagName)) {
                    return valueWrappers.get(COUNT).asLong();
                }
            }
        }
        return 0;
    }


    /**
     * gets the number of edges of the specified edge by edgeName,
     * you should execute submit job stats before executing this method,
     * if you pass in null means get all edges number.
     *
     * @param edgeName specified edge name
     * @return relationshipNumber
     * @throws UnsupportedEncodingException revert type is fail
     */
    public long relationshipNumber(String edgeName) throws UnsupportedEncodingException {
        ResultSet showStats = run(KeyWord.SHOW + " " + KeyWord.STATS);
        if (!showStats.isSucceeded()) {
            throw new ExecuteException(showStats.getErrorMessage());
        }
        if (showStats.rowsSize() != 0) {
            if (edgeName == null) {
                edgeName = EDGES;
            }
            for (int i = 0; i < showStats.rowsSize(); i++) {
                ResultSet.Record valueWrappers = showStats.rowValues(i);
                if (valueWrappers.get(NAME).asString().equals(edgeName)) {
                    return valueWrappers.get(COUNT).asLong();
                }
            }
        }
        return 0;
    }

    private ResultSet judgeExistVertexes(List<Vertex> vertices) {
        ArrayList<String> idList = new ArrayList<>();
        for (Vertex vertex : vertices) {
            idList.add(vertex.getVid() instanceof String
                ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString());
        }
        ResultSet findRemote =
            run(String.format(KeyWord.FETCH_PROP_ON + " " + KeyWord.ALL + " %s",
                String.join(",", idList)));
        if (!findRemote.isSucceeded()) {
            throw new ExecuteException(findRemote.getErrorMessage());
        }
        if (findRemote.isEmpty()) {
            throw new InitException("these vertexes not exist");
        }
        return findRemote;
    }

    private ResultSet judgeExistEdge(Relationship relationship) {
        ResultSet findRemote = run(String.format(KeyWord.FETCH_PROP_ON + " " + "`%s` %s->%s@%d ",
            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank()));
        if (!findRemote.isSucceeded()) {
            throw new ExecuteException(findRemote.getErrorMessage());
        }
        if (findRemote.isEmpty()) {
            throw new InitException("the edge " + relationship + " not exist");
        }
        return findRemote;
    }
}
