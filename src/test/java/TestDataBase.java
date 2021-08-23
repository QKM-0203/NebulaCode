/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.orm.datatype.DateTime;
import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.operator.DataType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

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
    Property propertyOne = new Property("class", DataType.INT8, false, 1);
    Property propertyTwo = new Property("grade", DataType.INT32, true, null);
    ArrayList<Property> properties = new ArrayList<>();
    Schema qkm5;
    Schema qkm6;
    Subgraph subgraph;
    Path path;

    {
        vertexValueOne.put("name", "qkm");
        vertexValueOne.put("age", 19);
        vertexValueOne.put("birth", new DateTime("2002-02-03 06:12:12:123"));
        vertexValueTwo.put("name", "sc");
        vertexValueTwo.put("age", 19);
        vertexValueTwo.put("birth", new DateTime("2001-04-07 06:12:12:123"));
        vertexValueThree.put("name", "sy");
        vertexValueThree.put("age", 20);
        vertexValueThree.put("birth", new DateTime("2001-08-13 06:12:12:123"));
        vertexValueFour.put("name", "yq");
        vertexValueFour.put("age", 21);
        vertexValueFour.put("birth", new DateTime("1999-12-25 06:12:12:123"));
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
        relationship24 = new Relationship(2, 4, "team", relationshipValueTwo, 1);
        relationship34 = new Relationship(3, 4, "work", null, 1);
        properties.add(propertyOne);
        properties.add(propertyTwo);
        qkm5 = new Schema("QKM6", properties, 0, null);
        qkm6 = new Schema("QKM5", null);
        ArrayList<Relationship> relationshipList = new ArrayList<>();
        relationshipList.add(relationship12);
        relationshipList.add(relationship32);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        subgraph = new Subgraph(vertices, relationshipList);
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment32 = new Segment(vertexThird, relationship32, vertexTwo);
        Segment segment24 = new Segment(vertexTwo, relationship24, vertexFour);
        Segment segment34 = new Segment(vertexThird, relationship34, vertexFour);
        List<Segment> segments = new ArrayList<>();
        segments.add(segment12);
        segments.add(segment32);
        segments.add(segment34);
        segments.add(segment24);
        path = new Path(segments);
    }
}
