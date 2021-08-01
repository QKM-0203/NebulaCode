/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.GraphService;
import com.vesoft.nebula.orm.entity.Property;
import com.vesoft.nebula.orm.entity.Schema;
import com.vesoft.nebula.orm.operator.DataType;
import java.util.ArrayList;
import java.util.Arrays;
import org.junit.Test;

public class TestUpdate {

    private GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1",9669),
            new HostAddress("127.0.0.1",9898)),
        "root","nebula",false);
    private Graph graph = graphService.getGraph("test");
    Property propertyOne = new Property("money", DataType.INT64,false,2000);
    Property propertyTwo = new Property("number", DataType.INT8,true,null);
    Property propertyThird = new Property("salve", DataType.FIXED_STRING,true,null);
    Property propertyFour = new Property("sex", DataType.STRING,false,"女");
    ArrayList<Property> properties = new ArrayList<>();
    Schema qkm5 = null;

    {
        properties.add(propertyOne);
        properties.add(propertyTwo);
        properties.add(propertyThird);
        properties.add(propertyFour);
        qkm5 = new Schema("QKM5",properties,10,"money",0);
    }

    @Test
    public void testUpdatePropForSchema() {
        graph.createSchema(qkm5);
        assert qkm5.getGraph() == graph;
        assert qkm5.getPropType("salve").equals("FIXED_STRING");
        assert qkm5.getPropType("sex").equals("STRING");
        Property propertyThirdNew = new Property("salve", DataType.STRING,true,null);
        Property propertyFourNew = new Property("sex", DataType.INT8,false,0);
        ArrayList<Property> propertiesNew = new ArrayList<>();
        propertiesNew.add(propertyThirdNew);
        propertiesNew.add(propertyFourNew);
        qkm5.modifyProp(propertiesNew);
        assert qkm5.getPropType("salve").equals("STRING");
        assert qkm5.getPropType("sex").equals("INT8");
    }
}
