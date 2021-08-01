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

public class TestCreate {
    private GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1",9669),
            new HostAddress("127.0.0.1",9898)),
        "root","nebula",false);
    private Graph graph = graphService.getGraph("test");
    HashMap<String, HashMap<String, Object>> vertexMapOne = new HashMap<>();
    HashMap<String, HashMap<String, Object>> vertexMapTwo = new HashMap<>();
    HashMap<String, Object> vertexValueOne = new HashMap<>();
    HashMap<String, Object> vertexValueTwo = new HashMap<>();
    HashMap<String, Object> relationshipValueOne = new HashMap<>();
    Vertex vertexOne = null;
    Vertex vertexTwo = null;
    Vertex vertexThird = null;
    Relationship relationship12 = null;
    Relationship relationship23 = null;
    Property propertyOne = new Property("money", DataType.INT64,false,2000);
    Property propertyTwo = new Property("number", DataType.INT8,true,null);
    ArrayList<Property> properties = new ArrayList<>();
    Schema qkm5 = null;
    Schema qkm6 = null;


    {
        vertexMapOne.put("QKM1",null);
        vertexValueOne.put("name","qkm");
        vertexValueOne.put("age",13);
        vertexValueTwo.put("salve",20000);
        vertexValueTwo.put("sex","女");
        vertexMapOne.put("QKM2",vertexValueOne);
        vertexMapTwo.put("QKM3",null);
        vertexMapTwo.put("QKM4",vertexValueTwo);
        vertexOne = new Vertex("1",vertexMapOne);
        vertexTwo = new Vertex("2",vertexMapOne);
        vertexThird = new Vertex("3",vertexMapTwo);
        relationshipValueOne.put("teamName","china");
        relationshipValueOne.put("teacher","Sun");
        relationship12 = new Relationship("1", "2", "team", relationshipValueOne, 1);
        relationship23 = new Relationship("2", "3", "work", null, 1);
        properties.add(propertyOne);
        properties.add(propertyTwo);
        qkm5 = new Schema("QKM5",properties,10,"money",0);
        qkm6 = new Schema("QKM6",null,1);
    }

    @Test
    public void testCreateSpace()  {
        Space testOne = new Space("test",10,1, DataType.FIXED_STRING.setLength(30));
        Space testTwo = new Space("test",10,1, DataType.INT64);
        graphService.createSpace(testOne);
        graphService.createSpace(testTwo);
    }


    @Test
    public void testCreateVertex() {
        graph.create(vertexOne);
        graph.create(vertexTwo);
        assert vertexOne.getGraph() == graph;
        assert vertexTwo.getGraph() == graph;
    }

    @Test
    public void testCreateRelationship() {
        graph.create(relationship12);
        graph.create(relationship23);
        assert relationship12.getGraph() == graph;
        assert relationship23.getGraph() == graph;
    }



    @Test
    public void testCreateSubgraph() {
        ArrayList<Relationship> relationshipList = new ArrayList<>();
        relationshipList.add(relationship12);
        relationshipList.add(relationship23);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        Subgraph subgraph = new Subgraph(vertices, relationshipList);
        graph.create(subgraph);
        assert subgraph.getVertexes().get(0).getGraph() == graph;
        assert subgraph.getRelationships().get(0).getGraph() == graph;
    }

    @Test
    public void testCreatePath() {
        Part part12 = new Part(vertexOne,relationship12,vertexTwo);
        Part part23 = new Part(vertexTwo,relationship23,vertexThird);
        List<Part> parts = new ArrayList<>();
        parts.add(part12);
        parts.add(part23);
        Path path = new Path(parts);
        graph.create(path);
        assert path.getStartVertex().getGraph() == graph;
        assert path.getRelationships().get(0).getGraph() == graph;
    }

    @Test
    public void testCreateSchema() {
        graph.createSchema(qkm5);
        graph.createSchema(qkm6);
        assert qkm5.getGraph() == graph;
        assert qkm6.getGraph() == graph;
    }

    @Test
    public void testAddForSchema() {
        graph.createSchema(qkm5);
        assert qkm5.getGraph() == graph;
        ArrayList<Property> addProperties = new ArrayList<>();
        Property propertyThird = new Property("salve", DataType.DOUBLE,true,10000.0);
        addProperties.add(propertyThird);
        qkm5.addProp(addProperties);
        assert qkm5.getPropType("salve").equals("DOUBLE");
    }




}
