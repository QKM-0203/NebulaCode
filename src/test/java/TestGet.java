/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.TimeWrapper;
import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.operator.DataType;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.junit.Test;

public class TestGet {
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
    HashMap<String, Object> relationshipValueTwo = new HashMap<>();
    Vertex vertexOne;
    Vertex vertexTwo;
    Vertex vertexThird;
    Vertex vertexFour;
    Relationship relationship12;
    Relationship relationship23;
    Relationship relationship43;
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
        vertexFour = new Vertex("4", vertexMapTwo);
        relationshipValueOne.put("teamName", "china");
        relationshipValueOne.put("teacher", "Sun");
        relationshipValueTwo.put("teacher", "MaLong");
        relationshipValueTwo.put("teamName", "Linux");
        relationship12 = new Relationship("1", "2", "team", relationshipValueOne, 1);
        relationship23 = new Relationship("2", "3", "work", null, 1);
        relationship43 = new Relationship("2", "3", "team", relationshipValueTwo, 1);
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
        vertices.add(vertexFour);
        subgraph = new Subgraph(vertices, relationshipList);
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment23 = new Segment(vertexTwo, relationship23, vertexThird);
        Segment segment43 = new Segment(vertexTwo, relationship23, vertexThird);
        List<Segment> segments = new ArrayList<>();
        segments.add(segment12);
        segments.add(segment23);
        path = new Path(segments);
    }

    @Test
    public void testGetTags() {
        List<String> tags = graph.getTags();
        assert tags.toString().equals("[QKM1, QKM2, QKM3, QKM4, "
            + "QKM6]");
    }

    @Test
    public void testGetEdges() {
        List<String> tags = graph.getEdges();
        assert tags.toString().equals("[QKM5, team, work]");
    }

    @Test
    public void testPullVertex() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
        HashMap<String, Object> vertexValue = new HashMap<>();
        vertexValue.put("name", "qkm");
        propMap.put("QKM2", vertexValue);
        Vertex vertex = new Vertex("2", propMap);
        graph.pull(vertex);
        assert vertex.hasTag("QKM3");
    }

    @Test
    public void testPullRelationship() throws UnsupportedEncodingException {
        HashMap<String, Object> edgeValue = new HashMap<>();
        edgeValue.put("teacher", "QI");
        Relationship relationship = new Relationship("1", "2", "team", edgeValue, 1);
        graph.pull(relationship);
        assert relationship.getPropMap().get("teacher").toString().equals("Sun");
    }

    @Test
    public void testPullSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        graph.run("upsert edge on team \"1\"->\"2\"@1 set teamName = \"XuXin\"");
        graph.run("upsert vertex on QKM4 \"3\" set sex = \"女\"");
        graph.pull(subgraph);
        System.out.println(subgraph);
        assert subgraph.getVertexes().get(2).getPropMap().get("QKM4")
            .get("sex").toString().equals("女");
        assert subgraph.getRelationships().get(0).getPropMap()
            .get("teamName").toString().equals("XuXin");
    }

}
