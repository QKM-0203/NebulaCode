/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.GraphService;
import java.util.Arrays;
import org.junit.Test;

public class TestConnect {
    @Test
    public void testConnect() {
        GraphService graphService = new GraphService(
            Arrays.asList(new HostAddress("127.0.0.1", 9669),
                new HostAddress("127.0.0.1", 9898)),
            "root", "nebula", false);
        Graph test = graphService.getGraph("test");
        assert test != null;
    }
}
