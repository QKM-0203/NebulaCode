import Operator.Type;
import com.vesoft.nebula.client.graph.NebulaPoolConfig;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import entity.*;
import operation.TagOperation;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Text {
    private static final Logger log = LoggerFactory.getLogger(Text.class);
    private static void printResult(ResultSet resultSet) throws UnsupportedEncodingException {
        List<String> colNames = resultSet.keys();
        // System.out.println(colNames.toString());
        for (String name : colNames) {
            System.out.printf("%15s |", name);
        }
        System.out.println();
        for (int i = 0; i < resultSet.rowsSize(); i++) {
            ResultSet.Record record = resultSet.rowValues(i);
            for (ValueWrapper value : record.values()) {
                if (value.isLong()) {
                    System.out.printf("%15s |", value.asLong());
                }
                if (value.isBoolean()) {
                    System.out.printf("%15s |", value.asBoolean());
                }
                if (value.isDouble()) {
                    System.out.printf("%15s |", value.asDouble());
                }
                if (value.isString()) {
                    System.out.printf("%15s |", value.asString());
                }
                if (value.isTime()) {
                    System.out.printf("%15s |", value.asTime());
                }
                if (value.isDate()) {
                    System.out.printf("%15s |", value.asDate());
                }
                if (value.isDateTime()) {
                    System.out.printf("%15s |", value.asDateTime());
                }
                if (value.isVertex()) {
                    System.out.printf("%15s |", value.asNode());
                }
                if (value.isEdge()) {
                    System.out.printf("%15s |", value.asRelationship());
                }
                if (value.isPath()) {
                    System.out.printf("%15s |", value.asPath());
                }
                if (value.isList()) {
                    System.out.printf("%15s |", value.asList());
                }
                if (value.isSet()) {
                    System.out.printf("%15s |", value.asSet());
                }
                if (value.isMap()) {
                    System.out.printf("%15s |", value.asMap());
                }
            }
            System.out.println();
        }
    }
   @Test
    public void test1() throws IOErrorException, UnsupportedEncodingException, InterruptedException {

       ConnectionProfile connectionProfile = new ConnectionProfile();
       Session session = connectionProfile.getSession(false);
       {

           String query = "GO FROM \"Tom\" OVER like "
                   + "YIELD $^.person.name, $^.person.age, like.likeness";
           ResultSet resp = session.execute(query);
           printResult(resp);
       }
       session.release();

   }
    @Test
    public void test2() throws IOErrorException, UnsupportedEncodingException, InterruptedException {

        NebulaPoolConfig nebulaPoolConfig = new NebulaPoolConfig();
        nebulaPoolConfig.setMaxConnSize(100);
        GraphService graphService =  new GraphService("127.0.0.1",9669,"root","1","test",nebulaPoolConfig);
        {
            String query = "describe tag QKM2";
            ResultSet resp = graphService.run(query);
            if (!resp.isSucceeded()) {
                log.error(String.format("Execute: `%s', failed: %s",
                        query, resp.getErrorMessage()));
                System.exit(1);
            }
            System.out.println(resp);
        }

    }

    @Test
    public void test_add_Tag() throws NoSuchFieldException, IllegalAccessException {
        Tag tag = new Tag("QKM3");
        TagOperation tagOperation = new TagOperation(new ConnectionProfile());
        tagOperation.create(tag);
    }

    @Test
    public void test_drop_Tag() throws NoSuchFieldException, IllegalAccessException {
        Tag tag = new Tag("QKM3");
        TagOperation tagOperation = new TagOperation(new ConnectionProfile());
        tagOperation.drop(tag);
    }


    @Test
    public void test_add_TagProp()  {
        Property property = new Property("name", Type.STRING,false,null);
        Property property1 = new Property("age", Type.INT8,false,null);
        ArrayList<Property> properties = new ArrayList<>();
        properties.add(property);
        properties.add(property1);
        Tag tag = new Tag("QKM2",properties);
    }

    @Test
    public void test_add_space() {
        Space space = new Space("QKM",10,1,Type.FIXED_STRING.setLength(30));
        Space space1 = new Space("QKM",10,1,Type.INT64);
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

    @Test
    public  void test_add_edge(){
        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
        HashMap<String, Object> propValue = new HashMap<>();
        propValue.put("name","asd");
        propValue.put("age",19);
        propMap.put("QKM2",propValue);
        //插入一条边，每条边有一个边类型
        Edge edge = new Edge("1","2",1,propMap);

        Edge edge1 = new Edge("1","2",propMap);




        //插入多条边，每条边有一个边类型
        Edge edge2 = new Edge("1","2",1,propMap);
        Edge edge3 = new Edge("1","2",propMap);

    }





}
