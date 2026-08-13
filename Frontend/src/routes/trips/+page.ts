import type { PageLoad } from './$types';
import axios from "axios"

type Trip = {
  id: number
  name: string
}

export const load: PageLoad = async ({ params }) => {
  try {
    const result =
      await axios.request<Trip[]>({
	url: "http://localhost:8081/trip",
	method: 'get',
      });
    console.log(result);
    return { trips: result.data };
  } catch (error) {
    console.log(error);
    return [];
  }
}
