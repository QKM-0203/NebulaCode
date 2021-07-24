/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
import Operator.DateType;
import entity.*;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

public class Text {


    @Test
    public void test_add_TagProp()  {
        Property property = new Property("name", DateType.STRING,false,null);
        Property property1 = new Property("age", DateType.INT8,false,null);
        ArrayList<Property> properties = new ArrayList<>();
        properties.add(property);
        properties.add(property1);
        Schema schema = new Schema("QKM2",properties);
    }

    @Test
    public void test_add_space() {
        Space space = new Space("QKM",10,1, DateType.FIXED_STRING.setLength(30));
        Space space1 = new Space("QKM",10,1, DateType.INT64);
    }


    @Test
    public void test_add_vertex(){
        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
        HashMap<String, Object> propValue = new HashMap<>();
        propValue.put("name","asd");
        propValue.put("age",19);
        propMap.put("QKM2",propValue);
        //插入一个点，该点含有一个标签
        Vertex vertex1 = new Vertex(1, propMap);
        Vertex vertex2 = new Vertex("1", propMap);


        //插入一个点，每个点都有多个标签
        Vertex vertex4 = new Vertex(1, propMap);



    }







}
