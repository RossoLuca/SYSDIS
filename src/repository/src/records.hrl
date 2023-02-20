-record(delivery,{
        id,
        pid,
        state,
        start_x,
        start_y,
        current_x,
        current_y,
        end_x,
        end_y,
        fallen
    }).

-record(table_ids, {
        table_name,
        last_id
    }).