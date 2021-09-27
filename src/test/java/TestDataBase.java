/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.ngqlbuilder.entity.*;
import com.vesoft.nebula.ngqlbuilder.operator.DataType;
import com.vesoft.nebula.ngqlbuilder.timetype.DateTime;
import java.util.*;

public class TestDataBase {
    public GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1", 9669),
            new HostAddress("127.0.0.1", 9898)),
        "root", "nebula", false);
    public Graph graph = graphService.getGraph("Test");
    HashMap<String, HashMap<String, Object>> vertexMapOne = new HashMap<>();
    HashMap<String, HashMap<String, Object>> vertexMapTwo = new HashMap<>();
    HashMap<String, HashMap<String, Object>> vertexMapThree = new HashMap<>();
    HashMap<String, HashMap<String, Object>> vertexMapFour = new HashMap<>();
    HashMap<String, Object> vertexValueOne = new HashMap<>();
    HashMap<String, Object> vertexValueTwo = new HashMap<>();
    HashMap<String, Object> vertexValueThree = new HashMap<>();
    HashMap<String, Object> vertexValueFour = new HashMap<>();
    HashMap<String, Object> relationshipValueOne = new HashMap<>();
    HashMap<String, Object> relationshipValueTwo = new HashMap<>();
    Vertex vertexOne;
    Vertex vertexTwo;
    Vertex vertexThird;
    Vertex vertexFour;
    Relationship relationship12;
    Relationship relationship32;
    Relationship relationship24;
    Relationship relationship34;
    Relationship relationship23;
    Relationship relationship13;
    Property propertyOne = new Property("name", DataType.STRING.setLength(10), true, null);
    Property propertyTwo = new Property("age", DataType.INT, true, null);
    Property propertyThird = new Property("birth", DataType.DATETIME, true, null);
    Property propertyFour = new Property("teacherName", DataType.STRING.setLength(10), true, null);
    Property propertyFive = new Property("object", DataType.STRING.setLength(20), true, null);
    ArrayList<Property> tagProperties = new ArrayList<>();
    ArrayList<Property> edgeProperties = new ArrayList<>();
    Schema qkm1;
    Schema qkm2;
    Schema team;
    Schema work;
    Subgraph subgraph;
    Segment segment12;
    Segment segment32;
    Segment segment24;
    Segment segment34;
    Path path;

    {
        vertexValueOne.put("name", "qkm");
        vertexValueOne.put("age", 19);
        vertexValueOne.put("birth", new DateTime("2002-02-03T06:12:12:123"));
        vertexValueTwo.put("name", "sc");
        vertexValueTwo.put("age", 19);
        vertexValueTwo.put("birth", new DateTime("2001-04-07T06:12:12:123"));
        vertexValueThree.put("name", "sy");
        vertexValueThree.put("age", 20);
        vertexValueThree.put("birth", new DateTime("2001-08-13T06:12:12:123"));
        vertexValueFour.put("name", "yq");
        vertexValueFour.put("age", 21);
        vertexValueFour.put("birth", new DateTime("1999-12-25T06:12:12:123"));
        vertexMapOne.put("QKM1", null);
        vertexMapOne.put("QKM2", vertexValueOne);
        vertexMapTwo.put("QKM1", null);
        vertexMapTwo.put("QKM2", vertexValueTwo);
        vertexMapThree.put("QKM1", null);
        vertexMapThree.put("QKM2", vertexValueThree);
        vertexMapFour.put("QKM1", null);
        vertexMapFour.put("QKM2", vertexValueFour);
        vertexOne = new Vertex(1, vertexMapOne);
        vertexTwo = new Vertex(2, vertexMapTwo);
        vertexThird = new Vertex(3, vertexMapThree);
        vertexFour = new Vertex(4, vertexMapFour);
        relationshipValueOne.put("teacherName", "qkm");
        relationshipValueOne.put("object", "math");
        relationshipValueTwo.put("teacherName", "sc");
        relationshipValueTwo.put("object", "chinese");
        relationship12 = new Relationship(1, 2, "team", relationshipValueOne, 1);
        relationship32 = new Relationship(3, 2, "work", null, 1);
        relationship13 = new Relationship(1, 3, "work", null, 0);
        relationship24 = new Relationship(2, 4, "team", relationshipValueTwo, 1);
        relationship34 = new Relationship(3, 4, "work", null, 1);
        relationship23 = new Relationship(2, 3, "team", relationshipValueOne, 0);
        tagProperties.add(propertyOne);
        tagProperties.add(propertyTwo);
        tagProperties.add(propertyThird);
        edgeProperties.add(propertyFour);
        edgeProperties.add(propertyFive);
        qkm2 = new Schema("QKM2", tagProperties, 0, null);
        qkm1 = new Schema("QKM1", null);
        team = new Schema("team", edgeProperties);
        work = new Schema("work", null);
        ArrayList<Relationship> relationshipList = new ArrayList<>();
        relationshipList.add(relationship12);
        relationshipList.add(relationship32);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        subgraph = new Subgraph(vertices, relationshipList);
        segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        segment32 = new Segment(vertexThird, relationship32, vertexTwo);
        segment24 = new Segment(vertexTwo, relationship24, vertexFour);
        segment34 = new Segment(vertexThird, relationship34, vertexFour);
        List<Segment> segments = new ArrayList<>();
        segments.add(segment12);
        segments.add(segment32);
        segments.add(segment34);
        segments.add(segment24);
        path = new Path(segments);
    }
}
