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
            for (Vertex vertex : vertices) {
                ResultSet resultSet = run(String.format("DELETE VERTEX %s",
                    vertex.getVid() instanceof String
                        ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString()));
                if (!resultSet.isSucceeded()) {
                    throw new ExecuteException(resultSet.getErrorMessage());
                }
            }
        }
        if (relationships.size() != 0) {
            HashMap<String, List<Relationship>> joinSameEdgeRelationships =
                Util.joinSameEdgeRelationships(relationships, 1);
            ArrayList<String> relationshipString = new ArrayList<>();
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
                    throw new ExecuteException(resultSet.getErrorMessage());
                }
            }
        }
    }


    /**
     * The merge method only judges according to the passed in parameters
     * schemaName,so it is only part of the condition matching.
     *
     * <p>For a point, if the tag exists, update the attribute value of the corresponding tag,
     * otherwise insert the point.</p>
     * <p>For edges, existence is to update the attribute value, otherwise it is to insert.</p>
     * <p>if you merge relationship,you can not pass in schemaName,because relationship object
     * has edgeName,if not exist this method can create,otherwise update attribute value</p>
     *
     * <p>If you pass in a subgraph or path, the schemaName
     * you pass in is applied to all points in the subgraph or path</p>
     *
     * @param graphObject graphObject can be Vertex or Relationship or Subgraph or path.
     */
    public void merge(Object graphObject, String... schemaName) {
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        Util.judgeGraphObject(vertices, relationships, graphObject);
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                if (relationship.getPropMap() == null) {
                    ResultSet remoteRelationship = run(String.format("FETCH PROP ON `%s` %s->%s@%d",
                        relationship.getEdgeName(), relationship.getStartVid() instanceof String
                            ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                        relationship.getEndVid() instanceof String
                            ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                        relationship.getRank()));
                    if (remoteRelationship.rowsSize() == 0) {
                        create(relationship);
                    }
                } else {
                    String updateEdgeValue = Encoding.updateSchemaValue(relationship.getPropMap());
                    ResultSet resultSet = run(String.format("UPSERT EDGE ON `%s` %s->%s@%d SET %s",
                        relationship.getEdgeName(), relationship.getStartVid() instanceof String
                            ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                        relationship.getEndVid() instanceof String
                            ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                        relationship.getRank(), updateEdgeValue));
                    if (!resultSet.isSucceeded()) {
                        throw new ExecuteException(resultSet.getErrorMessage()
                            + "\n the tag " + schemaName[0] + " corresponding to "
                            + relationship + " merge fail");
                    }
                }

            }
        }
        if (vertices.size() != 0) {
            for (Vertex vertex : vertices) {
                ResultSet resultSet = run("FETCH PROP ON `" + schemaName[0]
                    + "`" + (vertex.getVid() instanceof String
                    ? "\"" + vertex.getVid() + "\"" : vertex.getVid()));
                if (resultSet.rowsSize() == 0) {
                    create(vertex);
                } else {
                    String updateTagValue =
                        Encoding.updateSchemaValue(vertex.getPropMap().get(schemaName[0]));
                    ResultSet result = run(String.format("UPSERT VERTEX ON `%s` %s SET %s",
                        schemaName[0], vertex.getVid() instanceof String
                            ? "\"" + vertex.getVid() + "\"" : vertex.getVid(), updateTagValue));
                    if (!result.isSucceeded()) {
                        throw new ExecuteException(resultSet.getErrorMessage()
                            + "\n the tag " + schemaName[0] + " corresponding to "
                            + vertex + " merge fail");
                    }
                }
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
                idVertexMap.put(vertex.getVid() instanceof String
                    ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString(), vertex);
            }
            ResultSet vertexResultSet =
                run(String.format("FETCH PROP ON * %s",
                    String.join(",", new ArrayList<>(idVertexMap.keySet()))));
            if (vertexResultSet.isSucceeded()) {
                List<ValueWrapper> remoteVertices = vertexResultSet.colValues("vertices_");
                for (ValueWrapper remoteVertex : remoteVertices) {
                    Node node = remoteVertex.asNode();
                    if (idVertexMap.get(node.getId().toString()) != null) {
                        Vertex vertex = idVertexMap.get(node.getId().toString());
                        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
                        for (String tagName : node.tagNames()) {
                            propMap.put(tagName, new HashMap<>(node.properties(tagName)));
                        }
                        vertex.setPropMap(propMap);
                    }
                }
            } else {
                throw new ExecuteException(vertexResultSet.getErrorMessage());
            }
        }
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                ResultSet relationshipResultSet = run(String.format("FETCH PROP ON `%s` %s->%s@%d",
                    relationship.getEdgeName(), relationship.getStartVid() instanceof String
                        ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                    relationship.getEndVid() instanceof String
                        ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                    relationship.getRank()));
                List<ValueWrapper> remoteEdges = relationshipResultSet.colValues("edges_");

                if (relationshipResultSet.isSucceeded()) {
                    relationship.setPropMap(
                        new HashMap<>(remoteEdges.get(0).asRelationship().properties()));
                } else {
                    throw new ExecuteException(relationshipResultSet.getErrorMessage());
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
                ResultSet resultSet = run("FETCH PROP ON * "
                    + (vertex.getVid() instanceof String
                    ? "\"" + vertex.getVid() + "\"" : vertex.getVid().toString()));
                List<ValueWrapper> remoteVertex = resultSet.colValues("vertices_");
                if (remoteVertex.size() == 0) {
                    throw new ExecuteException("the vertex " + vertex
                        + " is not found at remote database");
                }
                List<String> remoteTags = remoteVertex.get(0).asNode().labels();
                Set<String> localTags = vertex.getPropMap().keySet();
                //add tags or update tag
                if (localTags.equals(new HashSet<>(remoteTags))
                    || remoteTags.containsAll(localTags)
                    || localTags.removeAll(new HashSet<>(remoteTags))) {
                    create(vertex);
                }
                //delete tag,TODO: depends on  https://github.com/vesoft-inc/nebula-graph/pull/1303
                if (remoteTags.removeAll(localTags)) {
                    System.out.println(remoteTags);
                }
            }
        }
        if (relationships.size() != 0) {
            for (Relationship relationship : relationships) {
                ResultSet findRemote = run(String.format("FETCH PROP ON `%s` %s->%s@%d ",
                    relationship.getEdgeName(), relationship.getStartVid() instanceof String
                        ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                    relationship.getEndVid() instanceof String
                        ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                    relationship.getRank()));
                List<ValueWrapper> remoteEdge = findRemote.colValues("edges_");
                if (remoteEdge.size() == 0) {
                    throw new ExecuteException("the relationship "
                        + relationship + " is not found at remote database");
                }
                if (relationship.getPropMap() != null) {
                    String updateEdgeValue = Encoding.updateSchemaValue(relationship.getPropMap());
                    ResultSet updateRemote =
                        run(String.format("UPSERT EDGE ON `%s` %s->%s@%d SET %s",
                            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                            relationship.getEndVid() instanceof String
                                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
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
        ResultSet resultSet = run(String.format("CREATE TAG INDEX %s ON %s(%s)",
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
        ResultSet resultSet = run(String.format("CREATE EDGE INDEX %s ON %s(%s)",
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
            .forEach(tagName -> tagNames.add(tagName.toString()));
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
            .forEach(tagName -> edgeNames.add(tagName.toString()));
        return edgeNames;
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
     * if (edgeName == null) {
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
        ResultSet resultSet;
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
        ResultSet resultSet;
        resultSet = run("DROP EDGE " + "`" + edgeName + "`");
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
    }

}
