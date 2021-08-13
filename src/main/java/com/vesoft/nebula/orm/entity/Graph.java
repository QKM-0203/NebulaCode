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
import com.vesoft.nebula.orm.ngql.Encoding;
import com.vesoft.nebula.orm.util.Util;

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

    protected Graph(String spaceName, Session session) {
        this.session = session;
        ResultSet result;
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
        HashMap<String, List<Vertex>> joinSameTagVertices = Util.joinSameTagVertices(vertices);
        if (vertices.size() != 0) {
            ArrayList<String> vertexValues = new ArrayList<>();
            ArrayList<Vertex> successVertexes = new ArrayList<>();
            for (String tagJoin : joinSameTagVertices.keySet()) {
                List<Vertex> vertexList = joinSameTagVertices.get(tagJoin);
                for (Vertex vertex : vertexList) {
                    vertexValues.add(Encoding.joinVertexValue(vertex));
                }
                ResultSet resultSet = run(String.format("INSERT VERTEX %s VALUES %s",
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
                ResultSet resultSet = run(String.format("INSERT EDGE %s VALUES %s",
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
                ResultSet resultSet = run(String.format("DELETE VERTEX %s",
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
                ResultSet resultSet = run(String.format("DELETE EDGE `" + edgeName + "` %s",
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
     * schemaName and propName,so it is only part of the condition matching.
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
            if (schema[0] == null || schema[1] == null) {
                throw new ExecuteException("tagName and attribute name is "
                    + "a condition of merge,so cannot null");
            }
            ResultSet resultSet = run("DESC TAG " + schema[0]);
            if (resultSet.isSucceeded()) {
                List<ValueWrapper> field = resultSet.colValues("Field");
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
            if (vertexResultSet.rowsSize() != 0) {
                List<ValueWrapper> remoteVertices = vertexResultSet.colValues(VERTICES_);
                for (ValueWrapper remoteVertex : remoteVertices) {
                    Node node = remoteVertex.asNode();
                    Vertex vertex = idVertexMap.get(node.getId().asString());
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
        }
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                ResultSet resultSet = judgeExistEdge(relationship);
                List<ValueWrapper> remoteEdges = resultSet.colValues(EDGES_);
                if (remoteEdges.size() != 0) {
                    HashMap<String, ValueWrapper> properties =
                        remoteEdges.get(0).asRelationship().properties();
                    HashMap<String, Object> edgeMap = new HashMap<>();
                    Util.judgeValueWrapper(edgeMap, properties);
                    relationship.setPropMap(edgeMap);
                    relationship.setGraph(this);
                }
            }
        }
    }

    /**
     * push local data to remote database.
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
            for (Vertex vertex : vertices) {
                ResultSet resultSet = judgeExistVertex(vertex);
                List<ValueWrapper> remoteVertex = resultSet.colValues(VERTICES_);
                if (remoteVertex.size() == 0) {
                    continue;
                }
                List<String> remoteTags = remoteVertex.get(0).asNode().labels();
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
                if (remoteEdge.size() == 0) {
                    throw new ExecuteException("the relationship "
                        + relationship
                        + " is not found at remote database");
                }
                if (relationship.getPropMap() != null) {
                    if (relationship.getPropMap() != null
                        && relationship.getPropMap().size() != 0) {
                        String updateEdgeValue = Encoding
                            .updateSchemaValue(relationship.getPropMap());
                        ResultSet updateRemote =
                            run(String.format("UPSERT EDGE ON `%s` %s->%s@%d SET %s",
                                relationship.getEdgeName(),
                                relationship.getStartVid() instanceof String
                                    ? "\"" + relationship.getStartVid()
                                    + "\"" : relationship.getStartVid(),
                                relationship.getEndVid() instanceof String
                                    ? "\"" + relationship.getEndVid()
                                    + "\"" : relationship.getEndVid(),
                                relationship.getRank(), updateEdgeValue));
                        if (!updateRemote.isSucceeded()) {
                            throw new ExecuteException(updateRemote.getErrorMessage()
                                + "\n the edge " + relationship.getEdgeName()
                                + " corresponding to  " + relationship + " push fail");
                        }
                    }
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
                               HashMap<String, Integer> propList) {
        ResultSet resultSet = run(String.format("CREATE TAG INDEX IF NOT EXISTS %s ON %s(%s)",
            indexName, tagName, (Encoding.joinIndexProp(propList)) == null
                ? "" : Encoding.joinIndexProp(propList)));
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
     * according to the given attribute map, {@code Integer} is index length,
     * only dataType is String has index length,so other pass in null.
     *
     * @param edgeName  edgeName
     * @param indexName indexName
     * @param propList  indexList
     */
    public void createEdgeIndex(String edgeName, String indexName,
                                HashMap<String, Integer> propList) {
        ResultSet resultSet = run(String.format("CREATE EDGE INDEX IF NOT EXISTS %s ON %s(%s)",
            indexName, edgeName, (Encoding.joinIndexProp(propList)) == null
                ? "" : Encoding.joinIndexProp(propList)));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        ResultSet rebuildIndex = run("REBUILD EDGE INDEX " + indexName);
        if (!rebuildIndex.isSucceeded()) {
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
        run("SHOW TAGS").colValues("Name")
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
        run("SHOW EDGES").colValues("Name")
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
        ResultSet resultSet = run("DROP TAG INDEX IF EXISTS " + indexName);
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
        ResultSet resultSet = run("DROP EDGE INDEX IF EXISTS " + indexName);
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
        String tagJoin = "CREATE TAG IF NOT EXISTS " + Encoding.joinSchema(schema);
        ResultSet resultSet = run(tagJoin);
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
        String edgeJoin = "CREATE EDGE IF NOT EXISTS " + Encoding.joinSchema(schema);
        ResultSet resultSet = run(edgeJoin);
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
            tagSentence.add("DROP TAG IF EXISTS " + "`" + tagName + "`");
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
            edgeSentence.add("DROP EDGE IF EXISTS " + "`" + edgeName + "`");
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


    private ResultSet judgeExistVertexes(List<Vertex> vertices) {
        ArrayList<String> idList = new ArrayList<>();
        for (Vertex vertex : vertices) {
            idList.add(vertex.getVid() instanceof String
                ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString());
        }
        ResultSet findRemote =
            run(String.format("FETCH PROP ON * %s",
                String.join(",", idList)));
        if (!findRemote.isSucceeded()) {
            throw new ExecuteException(findRemote.getErrorMessage());
        }
        return findRemote;
    }


    private ResultSet judgeExistVertex(Vertex vertex) {
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertex);
        return judgeExistVertexes(vertices);
    }


    private ResultSet judgeExistEdge(Relationship relationship) {
        ResultSet findRemote = run(String.format("FETCH PROP ON `%s` %s->%s@%d ",
            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank()));
        if (!findRemote.isSucceeded()) {
            throw new ExecuteException(findRemote.getErrorMessage());
        }
        return findRemote;
    }

    private void updateVertex(Vertex vertex, String tagName) {
        String updateTagValue =
            Encoding.updateSchemaValue(vertex.getPropMap().get(tagName));
        ResultSet result = run(String.format("UPSERT VERTEX ON `%s` %s SET %s",
            tagName, vertex.getVid() instanceof String
                ? "\"" + vertex.getVid()
                + "\"" : vertex.getVid(), updateTagValue));
        if (!result.isSucceeded()) {
            throw new ExecuteException(result.getErrorMessage()
                + "\n the tag " + tagName + " corresponding to "
                + vertex + " update fail");
        }
    }
}
