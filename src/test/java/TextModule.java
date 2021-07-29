/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
import ngql.Encoding;
import operator.DataType;
import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import entity.*;
import org.junit.Test;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class TextModule {


    @Test
    public void test_add_TagProp()  {
        Property property = new Property("name", DataType.STRING,false,null);
        Property property1 = new Property("age", DataType.INT8,false,null);
        ArrayList<Property> properties = new ArrayList<>();
        properties.add(property);
        properties.add(property1);
        Schema schema = new Schema("QKM2",properties);
    }

    @Test
    public void test_add_space() {
        Space space = new Space("QKM",10,1, DataType.FIXED_STRING.setLength(30));
        Space space1 = new Space("QKM",10,1, DataType.INT64);
    }


    @Test
    public void test_add_vertex(){
        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
        HashMap<String, Object> propValue = new HashMap<>();
        HashMap<String, Object> propValue1 = new HashMap<>();
        propValue.put("name","asd");
        propValue.put("age","123");
        propValue1.put("345354",19);
        propValue1.put("354",56);
        propMap.put("QKM2",null);
        propMap.put("QKM3",null);
        //插入一个点，该点含有一个标签
        //("1" :palyer{age: 17, name: "qkm"} :team{name: "as", teacher: "45"})
        //Vertex{vid=1, propMap={QKM3={name=asd, age=19}, QKM2={name=asd, age=19}}}
        Vertex vertex1 = new Vertex("1", propMap);
        Vertex vertex2 = new Vertex("2", propMap);

        System.out.println(vertex1);

        //插入一个点，每个点都有多个标签
        Vertex vertex4 = new Vertex("4", propMap);
        Vertex vertex5 = new Vertex("5", propMap);
        Vertex vertex6 = new Vertex("6", propMap);
        Vertex vertex7 = new Vertex("7", propMap);
        Vertex vertex8 = new Vertex("8", propMap);

//        System.out.println(Encoding.vertexTagJoin(propMap));

//        System.out.println(vertex2);
//        System.out.println(vertex4);
//
//        ArrayList<Vertex> vertices = new ArrayList<>();
//        vertices.add(vertex4);
//        vertices.add(vertex5);
//        vertices.add(vertex6);

        Relationship relationship24 = new Relationship("2","4","asd",propValue,1);
        Relationship relationship45 = new Relationship("5","4","asd",null,1);
        Relationship relationship56 = new Relationship("6","5","asd",propValue,1);

//        System.out.println(relationship24);
        Part part24 = new Part(vertex2,relationship24,vertex4);
        Part part45 = new Part(vertex4,relationship45,vertex4);
        Part part56 = new Part(vertex5,relationship56,vertex6);
//
//
        ArrayList<Part> parts = new ArrayList<>();
        parts.add(part24);
        parts.add(part45);
        parts.add(part56);
        Path path = new Path(parts);
        System.out.println(path);
//        path.walk();
//        System.out.println(path.getVertices());
//        System.out.println(path.getStartNode());
//        System.out.println(path.getEndNode());
//        System.out.println(path);



    }

    @Test
    public void text4() throws IOErrorException, UnsupportedEncodingException {
        GraphService graphService = new GraphService(Arrays.asList(new HostAddress("127.0.0.1",9669)),
                "root","nebula",false);
        Graph test = graphService.getGraph("test");
        ResultSet run = test.run("match (v:player) return v;");
        System.out.println(run);
//        Space space = new Space("xxx",120,2,DateType.FIXED_STRING.setLength(30));
//        boolean space1 = graphService.createSpace(space);
    }


    @Test
    public void text6(){


    }






}
