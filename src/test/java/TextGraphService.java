import com.vesoft.nebula.client.graph.NebulaPoolConfig;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import entity.ConnectionProfile;
import entity.GraphService;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class TextGraphService {
    private static final Logger log = LoggerFactory.getLogger(TextGraphService.class);
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
            String query = "GO FROM \"Tom\" OVER like "
                    + "YIELD $^.person.name, $^.person.age, like.likeness";
            ResultSet resp = graphService.run(query);
            if (!resp.isSucceeded()) {
                log.error(String.format("Execute: `%s', failed: %s",
                        query, resp.getErrorMessage()));
                System.exit(1);
            }
            printResult(resp);
        }


    }

}
