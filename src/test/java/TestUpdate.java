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

public class TestUpdate {
    private GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1", 9669),
            new HostAddress("127.0.0.1", 9898)),
        "root", "nebula", false);
    private Graph graph = graphService.getGraph("test");
    HashMap<String, HashMap<String, Object>> vertexMapOne = new HashMap<>();
    HashMap<String, HashMap<String, Object>> vertexMapTwo = new HashMap<>();
    HashMap<String, Object> vertexValueOne = new HashMap<>();
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
        qkm5 = new Schema("QKM5", properties, 10, "money");
        qkm6 = new Schema("QKM6", null);
        ArrayList<Relationship> relationshipList = new ArrayList<>();
        relationshipList.add(relationship12);
        relationshipList.add(relationship23);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        subgraph = new Subgraph(vertices, relationshipList);
        Part part12 = new Part(vertexOne, relationship12, vertexTwo);
        Part part23 = new Part(vertexTwo, relationship23, vertexThird);
        List<Part> parts = new ArrayList<>();
        parts.add(part12);
        parts.add(part23);
        path = new Path(parts);
    }

    @Test
    public void testAddTagForVertex() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        vertexMapOne.put("QKM3", null);
        graph.push(vertexOne);
        assertHasTag("QKM3", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());

    }

    public void assertHasTag(String tagName, String vid) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().hasLabel(tagName);

    }

    @Test
    public void testUpdateTagValue() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        vertexValueOne.put("name", "QKM");
        graph.push(vertexOne);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());

    }

    public void assertTagPropValue(String tagName, String vid) throws UnsupportedEncodingException {

        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().properties(tagName).get("name").toString().equals("\"QKM\"");

    }

    @Test
    public void testUpdateRelationship() throws UnsupportedEncodingException {
        graph.create(relationship12);
        relationshipValueOne.put("teamName", "SC");
        graph.push(relationship12);
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
            .get(0).asRelationship().properties().get("teamName").toString().equals("\"SC\"");

    }

    @Test
    public void testUpdateSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        relationshipValueOne.put("teamName", "SC");
        vertexValueOne.put("name", "QKM");
        graph.push(subgraph);
        assertEdgePropValue(relationship12);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
    }

    @Test
    public void testUpdatePath() throws UnsupportedEncodingException {
        graph.create(path);
        relationshipValueOne.put("teamName", "SC");
        vertexValueOne.put("name", "QKM");
        graph.push(path);
        assertEdgePropValue(relationship12);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
    }
}
