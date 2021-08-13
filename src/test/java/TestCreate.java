/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.operator.DataType;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.junit.Test;

public class TestCreate {
    private GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1", 9669),
            new HostAddress("127.0.0.1", 9898)),
        "root", "nebula", false);
    private Graph graph = graphService.getGraph("test");
    HashMap<String, HashMap<String, Object>> vertexMapOne = new HashMap<>();
    HashMap<String, HashMap<String, Object>> vertexMapTwo = new HashMap<>();
    HashMap<String, Object> vertexValueOne = new HashMap<>();
    HashMap<String, Object> vertexValueOne1 = new HashMap<>();
    HashMap<String, Object> vertexValueTwo = new HashMap<>();
    HashMap<String, Object> relationshipValueOne = new HashMap<>();
    Vertex vertexOne;
    Vertex vertexTwo;
    Vertex vertexThird;
    Relationship relationship12;
    Relationship relationship23;
    Property propertyOne = new Property("money", DataType.INT64, false, 2000);
    Property propertyTwo = new Property("number", DataType.INT8, true, null);
    ArrayList<Property> properties = new ArrayList<>();
    Schema qkm5;
    Schema qkm6;
    Subgraph subgraph;
    Path path;

    {
        vertexMapOne.put("QKM1", null);
        vertexValueOne.put("name", "qkm");
        vertexValueOne.put("age", 12);
        vertexValueTwo.put("salve", 20000);
        vertexValueTwo.put("sex", "女");
        vertexMapOne.put("QKM2", vertexValueOne);
        vertexMapTwo.put("QKM3", null);
        vertexMapTwo.put("QKM4", vertexValueTwo);
        vertexOne = new Vertex("1", vertexMapOne);
        vertexTwo = new Vertex("2", vertexMapTwo);
        vertexThird = new Vertex("3", vertexMapTwo);
        relationshipValueOne.put("teamName", "china");
        relationshipValueOne.put("teacher", "Sun");
        relationship12 = new Relationship("1", "2", "team", relationshipValueOne, 1);
        relationship23 = new Relationship("2", "3", "work", null, 1);
        properties.add(propertyOne);
        properties.add(propertyTwo);
        qkm5 = new Schema("QKM5", properties, 0, null);
        qkm6 = new Schema("QKM6", null);
        ArrayList<Relationship> relationshipList = new ArrayList<>();
        relationshipList.add(relationship12);
        relationshipList.add(relationship23);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        subgraph = new Subgraph(vertices, relationshipList);
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment23 = new Segment(vertexTwo, relationship23, vertexThird);
        List<Segment> segments = new ArrayList<>();
        segments.add(segment12);
        segments.add(segment23);
        path = new Path(segments);
    }

    @Test
    public void testCreateSpace() {
        Space testOne = new Space("test1", 10, 1, DataType.FIXED_STRING.setLength(30));
        Space testTwo = new Space("test2", 10, 1, DataType.INT64);
        graphService.createSpace(testOne);
        graphService.createSpace(testTwo);
        assert null != graphService.getGraph("test1");
        assert null != graphService.getGraph("test2");
    }

    @Test
    public void testCreateVertex() {
        graph.create(vertexTwo);
        graph.create(vertexOne);
        assert vertexTwo.getGraph() == graph;
        assert vertexOne.getGraph() == graph;
        assert graph.exists(vertexOne);
        assert graph.exists(vertexTwo);
    }

    @Test
    public void testCreateRelationship() {
        graph.create(relationship12);
        graph.create(relationship23);
        assert relationship12.getGraph() == graph;
        assert relationship23.getGraph() == graph;
        assert graph.exists(relationship12);
        assert graph.exists(relationship23);
    }

    @Test
    public void testCreateSubgraph() {
        graph.create(subgraph);
        assert subgraph.getVertexes().get(0).getGraph() == graph;
        assert subgraph.getRelationships().get(0).getGraph() == graph;
        assert graph.exists(subgraph);
    }

    @Test
    public void testCreatePath() {
        graph.create(path);
        assert path.getStartVertex().getGraph() == graph;
        assert path.getRelationships().get(0).getGraph() == graph;
        assert graph.exists(path);
    }

    @Test
    public void testCreateSchema() {
        graph.createEdge(qkm5);
        graph.createTag(qkm6);
        assert graph.getEdges().contains("\"" + qkm5.getName() + "\"");
        assert graph.getTags().contains("\"" + qkm6.getName() + "\"");
    }

    @Test
    public void createEdgeIndex() {
        graph.createEdge(qkm5);
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("money", null);
        edgeIndexes.put("number", null);
        graph.createEdgeIndex("QKM5", "t_qkm5", edgeIndexes);
        assert graph.getEdgeIndexes("QKM5").contains("\"t_qkm5\"");
    }

    @Test
    public void createTagIndex() {
        graph.createTag(qkm6);
        HashMap<String, Integer> tagIndexes = new HashMap<>();
        graph.createTagIndex("QKM6", "t_qkm6", null);
        assert graph.getTagIndexes("QKM6").contains("\"t_qkm6\"");
    }

    @Test
    public void mergeAddVertex() {
        graph.delete(vertexOne);
        assert !graph.exists(vertexOne);
        graph.merge(vertexOne, "QKM5","name");
        graph.exists(vertexOne);
    }

    @Test
    public void mergeUpdateVertex() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        vertexValueOne.put("age", 19);
        graph.merge(vertexOne, "QKM2","age");
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
    }

    public void assertTagPropValue(String tagName, String vid) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().properties(tagName).get("age").toString().equals("19");

    }

    @Test
    public void mergeAddRelationship() {
        graph.delete(relationship12);
        assert !graph.exists(relationship12);
        graph.merge(relationship12);
        graph.exists(relationship12);
    }

    @Test
    public void mergeUpdateEdge() throws UnsupportedEncodingException {
        graph.create(relationship12);
        assert graph.exists(relationship12);
        relationshipValueOne.put("teamName", "China");
        graph.merge(relationship12);
        assertEdgePropValue(relationship12);
    }

    public void assertEdgePropValue(Relationship relationship) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run(String.format("FETCH PROP ON `%s` %s->%s@%d",
            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank()));
        assert resultSet.colValues("edges_")
            .get(0).asRelationship().properties().get("teamName").asString().equals("China");

    }

    @Test
    public void mergeSubgraph() throws UnsupportedEncodingException {
        graph.delete(vertexTwo);
        graph.delete(vertexThird);
        assert !vertexTwo.hasTag("QKM2");
        assert !vertexThird.hasTag("QKM2");
        assert !graph.exists(vertexOne);
        assert !graph.exists(vertexThird);
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        graph.pull(vertexOne);
        assert vertexOne.hasTag("QKM2");
        vertexOne.getPropMap().get("QKM2").put("age", 19);
        relationshipValueOne.put("teamName", "China");
        graph.merge(subgraph, "QKM2","name");
        assert graph.exists(vertexTwo);
        assert graph.exists(vertexThird);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
        assertEdgePropValue(relationship12);
    }

    @Test
    public void mergePath() throws UnsupportedEncodingException {
        graph.delete(vertexTwo);
        graph.delete(vertexThird);
        assert !vertexTwo.hasTag("QKM2");
        assert !vertexThird.hasTag("QKM2");
        assert !graph.exists(vertexOne);
        assert !graph.exists(vertexThird);
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        graph.pull(vertexOne);
        assert vertexOne.hasTag("QKM2");
        vertexOne.getPropMap().get("QKM2").put("age", 19);
        relationshipValueOne.put("teamName", "China");
        graph.merge(subgraph, "QKM2");
        assert graph.exists(vertexTwo);
        assert graph.exists(vertexThird);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
        assertEdgePropValue(relationship12);
        graph.merge(path, "QKM2","name");
    }
}
