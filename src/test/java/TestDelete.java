/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.operator.DataType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.junit.Test;

public class TestDelete {
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
    public void testDropSpace() {
        ArrayList<String> spaceNameList = new ArrayList<>();
        spaceNameList.add("test1");
        spaceNameList.add("test2");
        assert graphService.dropSpaces(spaceNameList);
    }

    @Test
    public void testDropVertex() {
        graph.create(vertexTwo);
        assert graph.exists(vertexTwo);
        graph.delete(vertexTwo);
        assert !graph.exists(vertexTwo);
    }

    @Test
    public void testDropRelationship() {
        graph.create(relationship12);
        assert graph.exists(relationship12);
        graph.delete(relationship12);
        assert !graph.exists(relationship12);
    }

    @Test
    public void testDropSubgraph() {
        graph.create(subgraph);
        assert graph.exists(subgraph);
        graph.delete(subgraph);
        assert !graph.exists(subgraph);
    }

    @Test
    public void testDropPath() {
        graph.create(path);
        assert graph.exists(path);
        graph.delete(path);
        assert !graph.exists(path);
    }

    @Test
    public void deleteTag() {
        graph.createTag(qkm6);
        assert graph.getTags().contains("\"" + qkm6.getName() + "\"");
        graph.dropTag("QKM6");
        assert !graph.getTags().contains("\"" + qkm6.getName() + "\"");
    }

    @Test
    public void deleteEdge() {
        graph.createEdge(qkm5);
        assert graph.getTags().contains("\"" + qkm5.getName() + "\"");
        graph.dropEdge("QKM5");
        assert !graph.getEdges().contains("\"" + qkm5.getName() + "\"");
    }

    @Test
    public void deleteEdgeIndex() {
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("money", 10);
        edgeIndexes.put("number", null);
        graph.createEdgeIndex("QKM5", "t_qkm5", edgeIndexes);
        assert graph.getEdgeIndexes("QKM5").contains("\"t_qkm5\"");
        graph.dropEdgeIndex("t_qkm5");
        assert !graph.getEdgeIndexes("QKM5").contains("\"t_qkm5\"");
    }

    @Test
    public void deleteTagIndex() {
        HashMap<String, Integer> tagIndexes = new HashMap<>();
        graph.createTagIndex("QKM6", "t_qkm6", null);
        assert graph.getTagIndexes("QKM6").contains("\"t_qkm6\"");
        graph.dropTagIndex("t_qkm6");
        assert !graph.getTagIndexes("QKM6").contains("\"t_qkm6\"");
    }

}
