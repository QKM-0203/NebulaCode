/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.Date;
import com.vesoft.nebula.DateTime;
import com.vesoft.nebula.Time;
import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.entity.Vertex;
import com.vesoft.nebula.orm.ngql.Encoding;
import java.util.HashMap;
import java.util.Objects;
import org.junit.Test;

public class TestEncoding {

    HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
    HashMap<String, Object> propValueOne = new HashMap<>();
    HashMap<String, Object> propValueTwo = new HashMap<>();
    short year = 2021;
    byte  month = 7;
    byte day = 29;
    byte hour = 13;
    byte minute = 34;
    byte sec = 56;
    byte microsec = 45;
    DateTime dateTime = new DateTime(year,month,day,hour,minute,sec,microsec);
    Date date = new Date(year,month,day);
    Time time = new Time(hour,minute,sec,microsec);
    Vertex vertex = null;

    {
        propValueOne.put("name","qkm");
        propValueOne.put("age",18);
        propValueOne.put("salve",1234567890);
        propValueOne.put("grade",123.45);
        propValueOne.put("birthday",date);
        propValueOne.put("birth",dateTime);
        propValueOne.put("money",123.5);
        propValueOne.put("etime",time);
        propValueTwo.put("bool",true);
        propValueTwo.put("null",null);
        propMap.put("QKM2", propValueOne);
        propMap.put("QKM3", propValueTwo);
        propMap.put("QKM4", null);
        vertex = new Vertex("1",propMap);

    }

    @Test
    public void testVertexTagJoin() {
        assert Encoding.vertexTagJoin(vertex.getPropMap()).equals("`QKM4`(),`QKM3`(`bool`,`null`),"
            + "`QKM2`(`birthday`,`money`,`grade`,`etime`,`name`,`birth`,`age`,`salve`)");
    }


    @Test
    public void testVertexValueJoin() {
        assert Objects.equals(Encoding.vertexValueJoin(vertex), "\"1\":(true,null,"
            + "date(\"2021-07-29\"),123.5,123.45,time(\"13:34:56:45\"),\"qkm\","
            + "datetime(\"2021-07-29T13:34:56:45\"),18,1234567890)");
    }


    @Test
    public void testRelationshipEdgeJoin() {
        Relationship relationship = new Relationship("1","2",
            "testEdgeName",propValueTwo,1);
        assert Encoding.relationshipEdgeJoin(relationship.getEdgeName(),
            relationship.getPropMap()).equals("`testEdgeName`(`bool`,`null`)");

    }

    @Test
    public void testRelationshipValueJoin() {
        Relationship relationship = new Relationship("1","2",
            "testEdgeName",propValueTwo,1);
        assert Encoding.relationshipValueJoin(relationship).equals("\"1\"->\"2\"@1:(true,null)");

    }
}

